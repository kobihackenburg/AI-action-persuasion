#!/usr/bin/env python3
"""
Direct-ask classifier for Study 2 conversations (SI Appendix Section 2.14).

For each conversation in the Study 2 raw CSV, asks Claude Sonnet 4.6 whether the
assistant made a "direct ask" to sign the petition, and writes one output row
per input row with the columns:

    row_index, participant_id, direct_ask, n_ask_messages,
    first_ask_message, ask_quote

Requires the unredacted transcripts (data/raw/full/study2_raw.csv). Running it
on the public, redacted CSV would classify "[REDACTED]" placeholders.

Usage:
    export ANTHROPIC_API_KEY=...
    python3 classifiers/direct_ask/classify.py --limit 20   # quick test
    python3 classifiers/direct_ask/classify.py              # full run
"""

import argparse
import asyncio
import csv
import json
import re
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from api import AsyncAskingClient
from config import (
    CACHE_FILE,
    CHECKPOINT_SIZE,
    CONVO_COLUMN,
    ID_COLUMN,
    MAX_CONCURRENT_REQUESTS,
    MODEL,
    NEW_COLUMNS,
    STUDY2_CSV,
)

try:
    csv.field_size_limit(sys.maxsize)
except OverflowError:
    csv.field_size_limit(2 ** 31 - 1)

# A single pipe-delimited turn, e.g. "3. ASSISTANT: ...". Mirrors the R parser
# in analysis/00_process_data.R.
TURN_RE = re.compile(r"^(\d+)\.\s*(ASSISTANT|USER):\s*(.*)$", re.DOTALL)


# ---- Conversation parsing / rendering ----------------------------------------

def parse_s2_conversation(text: Optional[str]) -> List[Dict]:
    """Split the pipe-delimited transcript into ordered {turn, role, text} dicts."""
    if text is None:
        return []
    text = str(text)
    if text.strip() == "":
        return []

    turns: List[Dict] = []
    for chunk in text.split(" | "):
        m = TURN_RE.match(chunk.strip())
        if not m:
            continue
        turns.append({
            "turn": int(m.group(1)),
            "role": m.group(2).lower(),
            "text": m.group(3).strip(),
        })
    return turns


def build_transcript(turns: List[Dict]) -> Tuple[str, int]:
    """
    Render turns for the classifier. Assistant messages are labelled
    "[Assistant #k]" with k the 1-based index over assistant messages only;
    user messages are labelled "[User]".

    Returns (transcript_string, n_assistant_messages).
    """
    lines: List[str] = []
    assistant_index = 0
    for t in turns:
        if t["role"] == "assistant":
            assistant_index += 1
            lines.append(f"[Assistant #{assistant_index}]: {t['text']}")
        else:
            lines.append(f"[User]: {t['text']}")
    return "\n\n".join(lines), assistant_index


# ---- Cache --------------------------------------------------------------------
# Results are keyed by positional row index ("__row_<i>") because participant_id
# is not unique in study2_raw.csv. Resuming from the cache is therefore only
# valid if the row order of the input CSV is unchanged.

def load_cache(path: Path) -> Dict[str, Dict]:
    results: Dict[str, Dict] = {}
    if not path.exists():
        return results
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(entry.get("result"), dict):
                results[entry["key"]] = entry["result"]
    return results


def append_cache(path: Path, entries: List[Dict]):
    if not entries:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "a", encoding="utf-8") as f:
        for e in entries:
            f.write(json.dumps(e, ensure_ascii=False) + "\n")


# ---- Classification -----------------------------------------------------------

def build_worklist(rows: List[Dict[str, str]]) -> Tuple[List[Tuple[str, str]], Dict[str, Dict]]:
    """
    Returns:
      work:    [(key, transcript)] for rows with at least one assistant message.
      trivial: {key: result} for rows with a conversation but no assistant
               messages (coded as no ask without an API call).
    Rows with no conversation text are skipped and left blank in the output.
    """
    work: List[Tuple[str, str]] = []
    trivial: Dict[str, Dict] = {}
    for i, r in enumerate(rows):
        convo = r.get(CONVO_COLUMN) or ""
        if convo.strip() == "":
            continue
        key = f"__row_{i}"
        transcript, n_assistant = build_transcript(parse_s2_conversation(convo))
        if n_assistant == 0:
            trivial[key] = {"direct_ask": False, "n_ask_messages": 0,
                            "first_ask_message": None, "ask_quote": None}
            continue
        work.append((key, transcript))
    return work, trivial


async def classify_all(pending: List[Tuple[str, str]], client: AsyncAskingClient,
                       model: str, results: Dict[str, Dict]) -> Tuple[int, int]:
    done = errors = 0
    total = len(pending)
    start = time.time()
    try:
        for bstart in range(0, total, CHECKPOINT_SIZE):
            batch = pending[bstart:bstart + CHECKPOINT_SIZE]
            outs = await asyncio.gather(*(client.classify_conversation(t) for _, t in batch),
                                        return_exceptions=True)
            ts = datetime.now().isoformat()
            entries: List[Dict] = []
            for (key, _), out in zip(batch, outs):
                if isinstance(out, Exception):
                    errors += 1
                    entries.append({"key": key, "error": str(out), "model": model, "timestamp": ts})
                    continue
                result, error, usage = out
                if error:
                    errors += 1
                    entries.append({"key": key, "error": error, "model": model, "usage": usage, "timestamp": ts})
                else:
                    done += 1
                    results[key] = result
                    entries.append({"key": key, "result": result, "model": model, "usage": usage, "timestamp": ts})
            append_cache(CACHE_FILE, entries)

            processed = min(bstart + CHECKPOINT_SIZE, total)
            rate = processed / max(time.time() - start, 1e-9)
            print(f"Progress: {processed}/{total} - {done} ok, {errors} err - "
                  f"ETA {(total - processed) / rate / 60:.1f} min")
    finally:
        await client.close()
    return done, errors


# ---- Output -------------------------------------------------------------------

def format_result(result: Optional[Dict]) -> Dict[str, str]:
    if result is None:
        return {c: "" for c in NEW_COLUMNS}
    first = result.get("first_ask_message")
    quote = result.get("ask_quote")
    return {
        "direct_ask": "True" if result.get("direct_ask") else "False",
        "n_ask_messages": str(int(result.get("n_ask_messages", 0) or 0)),
        "first_ask_message": "" if first is None else str(int(first)),
        "ask_quote": "" if quote is None else str(quote),
    }


def write_output(path: Path, rows: List[Dict[str, str]], results: Dict[str, Dict]):
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["row_index", ID_COLUMN] + NEW_COLUMNS)
        writer.writeheader()
        for i, r in enumerate(rows):
            writer.writerow({"row_index": i, ID_COLUMN: r.get(ID_COLUMN, ""),
                             **format_result(results.get(f"__row_{i}"))})


# ---- Main ---------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Direct-ask classifier for Study 2 conversations")
    parser.add_argument("--csv", default=str(STUDY2_CSV), help="Unredacted Study 2 raw CSV")
    parser.add_argument("--out", default=str(Path(__file__).resolve().parent / "output" / "study2_direct_ask.csv"),
                        help="Output CSV (one row per input row)")
    parser.add_argument("--model", default=MODEL)
    parser.add_argument("--concurrency", type=int, default=MAX_CONCURRENT_REQUESTS)
    parser.add_argument("--limit", type=int, default=None, help="Classify only the first N pending conversations")
    args = parser.parse_args()

    csv_path = Path(args.csv)
    if not csv_path.exists():
        sys.exit(f"Dataset not found: {csv_path}")

    with open(csv_path, "r", newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    if rows and CONVO_COLUMN not in rows[0]:
        sys.exit(f"Column '{CONVO_COLUMN}' not found in {csv_path}")

    results = load_cache(CACHE_FILE)
    work, trivial = build_worklist(rows)
    new_trivial = {k: v for k, v in trivial.items() if k not in results}
    results.update(new_trivial)
    append_cache(CACHE_FILE, [{"key": k, "result": v, "model": "local:no-assistant-messages",
                               "timestamp": datetime.now().isoformat()} for k, v in new_trivial.items()])

    pending = [w for w in work if w[0] not in results]
    n_cached = len(work) - len(pending)
    if args.limit is not None:
        pending = pending[:args.limit]
    print(f"Rows: {len(rows)} | to classify: {len(work)} | cached: {n_cached} | pending this run: {len(pending)}")

    if pending:
        client = AsyncAskingClient(max_concurrent=args.concurrency, model=args.model)
        done, errors = asyncio.run(classify_all(pending, client, args.model, results))
        print(f"Classification finished: {done} ok, {errors} errors")

    write_output(Path(args.out), rows, results)
    n_true = sum(1 for r in results.values() if r.get("direct_ask"))
    print(f"Wrote {args.out} | classified: {len(results)} | direct ask: {n_true}")


if __name__ == "__main__":
    main()
