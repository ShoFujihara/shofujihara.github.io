#!/usr/bin/env python3
"""Generate presentations.qmd from the [学会報告]/[その他報告] sections of _data.md.

Usage: add new entries (e.g. "[36] ...") to the relevant section in _data.md,
then run `python3 make_presentations.py` and `quarto render`.
Entries are listed newest first (reverse of the [NN] order in _data.md).
"""
import re

SECTIONS = [
    ("## [学会報告]（査読あり）", "## Conference Presentations (Peer-Reviewed)"),
    ("## [学会報告]（査読なし）", "## 学会報告（査読なし）"),
    ("## [その他報告]", "## Invited Talks and Other Presentations"),
]

txt = open("_data.md", encoding="utf-8").read()
data = {}
cur = None
for line in txt.split("\n"):
    if line.startswith("## "):
        cur = line.strip()
        data.setdefault(cur, [])
    elif cur is not None:
        if line.startswith("["):
            data[cur].append(line.strip())
        elif line.strip() and data[cur] and not line.startswith("#"):
            data[cur][-1] += " " + line.strip()

def strip_num(e):
    return re.sub(r"^\[\d+\]\s*", "", e)

out = ['---', 'title: "Presentations"', '---', '',
       'Conference presentations and invited talks, listed from the most recent. '
       'Peer-reviewed conference papers are listed first, followed by presentations '
       'at Japanese academic associations and other invited talks and seminars.', '']
for key, heading in SECTIONS:
    out += [heading, '']
    out += [f"{i}. {strip_num(e)}" for i, e in enumerate(reversed(data[key]), 1)]
    out.append('')
open("presentations.qmd", "w", encoding="utf-8").write("\n".join(out))
print({k: len(v) for k, v in data.items() if k in dict(SECTIONS)})
