#!/usr/bin/env python3
"""
Anthropic Messages API client and response parsing for the direct-ask classifier.

Each conversation is one request: the classification prompt as the user turn,
with the assistant turn prefilled with "{" to force a JSON object. The response
is parsed and normalised into a strict 4-field schema by normalize_result().
"""

import asyncio
import json
import time
from typing import Dict, List, Optional, Tuple

import aiohttp

from config import (
    ANTHROPIC_API_KEY,
    ANTHROPIC_BASE_URL,
    ANTHROPIC_VERSION,
    CLASSIFY_PROMPT,
    MAX_CONCURRENT_REQUESTS,
    MAX_REQUESTS_PER_MINUTE,
    MAX_TOKENS,
    MODEL,
    RETRY_BASE_DELAY,
    RETRY_MAX_ATTEMPTS,
    SYSTEM_PROMPT,
    TEMPERATURE,
)


# ---- Response parsing ---------------------------------------------------------

def _extract_text(result: Dict) -> str:
    """Concatenate the ``text`` blocks from a Messages API response."""
    parts: List[str] = []
    for block in result.get("content", []) or []:
        if isinstance(block, dict) and block.get("type") == "text":
            parts.append(block.get("text", "") or "")
    return "".join(parts)


def _strip_code_fences(text: str) -> str:
    """Remove markdown code fences from a model response, if present."""
    if "```json" in text:
        start = text.find("```json") + 7
        end = text.find("```", start)
        if end != -1:
            return text[start:end].strip()
    if "```" in text:
        start = text.find("```") + 3
        end = text.find("```", start)
        if end != -1:
            return text[start:end].strip()
    return text.strip()


def _coerce_bool(value) -> Optional[bool]:
    """Coerce a model-provided value to a strict bool, or None if unknown."""
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        return bool(value)
    if isinstance(value, str):
        v = value.strip().lower()
        if v in ("true", "yes", "1"):
            return True
        if v in ("false", "no", "0"):
            return False
    return None


def _coerce_int(value) -> Optional[int]:
    """Coerce a model-provided value to an int, or None if not parseable."""
    if value is None:
        return None
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    if isinstance(value, float):
        return int(value)
    if isinstance(value, str):
        v = value.strip()
        if v == "" or v.lower() in ("null", "none", "na", "n/a"):
            return None
        try:
            return int(float(v))
        except ValueError:
            return None
    return None


def normalize_result(parsed: Dict) -> Dict:
    """
    Validate/normalize the model's parsed JSON into the strict 4-field schema.

    Enforces internal consistency:
      - direct_ask is a bool
      - n_ask_messages is a non-negative int (0 if no ask)
      - first_ask_message is a positive int or None
      - ask_quote is a str or None
    """
    direct_ask = _coerce_bool(parsed.get("direct_ask"))
    n_ask = _coerce_int(parsed.get("n_ask_messages"))
    first_ask = _coerce_int(parsed.get("first_ask_message"))
    ask_quote = parsed.get("ask_quote")

    if isinstance(ask_quote, str):
        ask_quote = ask_quote.strip()
        if ask_quote == "" or ask_quote.lower() in ("null", "none"):
            ask_quote = None
    elif ask_quote is not None:
        ask_quote = str(ask_quote)

    # Reconcile fields so downstream columns are internally consistent.
    if direct_ask is None:
        # Infer from the count if the boolean was omitted/garbled.
        direct_ask = bool(n_ask) if n_ask is not None else False

    if not direct_ask:
        n_ask = 0
        first_ask = None
        ask_quote = None
    else:
        if n_ask is None or n_ask < 1:
            n_ask = 1
        if first_ask is not None and first_ask < 1:
            first_ask = None

    return {
        "direct_ask": bool(direct_ask),
        "n_ask_messages": int(n_ask) if n_ask is not None else 0,
        "first_ask_message": first_ask,
        "ask_quote": ask_quote,
    }


def _try_load_dict(s: str) -> Optional[Dict]:
    """json.loads that only succeeds for a dict; None otherwise."""
    try:
        obj = json.loads(s)
    except (json.JSONDecodeError, ValueError):
        return None
    return obj if isinstance(obj, dict) else None


def _first_json_object(text: str) -> Optional[Dict]:
    """
    Return the FIRST complete JSON object in ``text``, ignoring any trailing
    text after it (e.g. if the model appends further reasoning or a second
    object, the first, prefill-forced answer is used).
    """
    decoder = json.JSONDecoder()
    idx = text.find("{")
    while idx != -1:
        try:
            obj, _ = decoder.raw_decode(text, idx)
        except json.JSONDecodeError:
            obj = None
        if isinstance(obj, dict):
            return obj
        idx = text.find("{", idx + 1)
    return None


def _parse_classification(response_text: str, prefill: str = "") -> Tuple[Optional[Dict], Optional[Dict]]:
    """
    Parse a model response into a normalized result dict, or an error dict.

    ``prefill`` is prepended to the response, since the returned text is the
    continuation after the prefilled opening brace. Tries a direct load, then a
    fenced load, then the first complete ``{...}`` object.
    """
    if response_text is None:
        return None, {"error": "Empty response"}

    text = (prefill or "") + response_text

    parsed = _try_load_dict(text.strip())
    if parsed is None:
        parsed = _try_load_dict(_strip_code_fences(text))
    if parsed is None:
        parsed = _first_json_object(text)

    if parsed is None:
        return None, {"error": "Failed to parse JSON object", "response": text[:1000]}
    return normalize_result(parsed), None


# ---- Client -------------------------------------------------------------------

class AsyncAskingClient:
    """Async Anthropic Messages API client with concurrency control and retries."""

    def __init__(
        self,
        max_concurrent: int = MAX_CONCURRENT_REQUESTS,
        model: str = MODEL,
        api_key: Optional[str] = None,
        base_url: Optional[str] = None,
    ):
        self.model = model
        self.api_key = api_key or ANTHROPIC_API_KEY
        self.base_url = (base_url or ANTHROPIC_BASE_URL).rstrip("/")
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.session: Optional[aiohttp.ClientSession] = None
        self._request_times: List[float] = []
        self._rate_lock = asyncio.Lock()

    async def _ensure_session(self):
        if self.session is None or self.session.closed:
            self.session = aiohttp.ClientSession(
                headers={
                    "x-api-key": self.api_key,
                    "anthropic-version": ANTHROPIC_VERSION,
                    "content-type": "application/json",
                }
            )

    async def close(self):
        if self.session and not self.session.closed:
            await self.session.close()

    async def _rate_limit_wait(self):
        async with self._rate_lock:
            now = time.time()
            self._request_times = [t for t in self._request_times if now - t < 60]
            if len(self._request_times) >= MAX_REQUESTS_PER_MINUTE:
                sleep_time = 60 - (now - self._request_times[0]) + 0.1
                if sleep_time > 0:
                    await asyncio.sleep(sleep_time)
                now = time.time()
                self._request_times = [t for t in self._request_times if now - t < 60]
            self._request_times.append(now)

    async def _make_request(
        self,
        messages: List[Dict[str, str]],
    ) -> Tuple[Optional[str], Optional[Dict], Optional[Dict]]:
        """Single Messages API request with retries. Returns (text, error, usage)."""
        await self._ensure_session()

        payload = {
            "model": self.model,
            "max_tokens": MAX_TOKENS,
            "temperature": TEMPERATURE,
            "system": SYSTEM_PROMPT,
            "messages": messages,
        }

        attempts = 0
        while True:
            try:
                async with self.semaphore:
                    await self._rate_limit_wait()
                    async with self.session.post(
                        f"{self.base_url}/v1/messages",
                        json=payload,
                        timeout=aiohttp.ClientTimeout(total=180),
                    ) as response:
                        if response.status == 200:
                            result = await response.json()
                            return _extract_text(result), None, (result.get("usage") or {})

                        if response.status == 429 or response.status >= 500:
                            attempts += 1
                            if attempts >= RETRY_MAX_ATTEMPTS:
                                break
                            default_wait = RETRY_BASE_DELAY * (2 ** attempts)
                            wait = int(response.headers.get("Retry-After", default_wait)) \
                                if response.status == 429 else default_wait
                            print(f"HTTP {response.status}. Retrying in {wait}s...")
                            await asyncio.sleep(wait)
                            continue

                        text = await response.text()
                        return None, {"error": f"API error {response.status}: {text}",
                                      "status_code": response.status}, None

            except (asyncio.TimeoutError, aiohttp.ClientError) as e:
                attempts += 1
                if attempts >= RETRY_MAX_ATTEMPTS:
                    break
                wait = RETRY_BASE_DELAY * (2 ** attempts)
                print(f"Request error ({type(e).__name__}). Retrying in {wait}s...")
                await asyncio.sleep(wait)

        return None, {"error": "Max retries exceeded", "attempts": attempts}, None

    async def classify_conversation(self, transcript: str) -> Tuple[Optional[Dict], Optional[Dict], Optional[Dict]]:
        """Classify one rendered transcript. Returns (result, error, usage)."""
        messages = [
            {"role": "user", "content": CLASSIFY_PROMPT.format(transcript=transcript)},
            {"role": "assistant", "content": "{"},
        ]
        response_text, error, usage = await self._make_request(messages)
        if error:
            return None, error, usage
        result, perr = _parse_classification(response_text, prefill="{")
        return result, perr, usage
