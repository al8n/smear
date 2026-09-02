#!/usr/bin/env python3
"""Print the executable a named cargo target was built to, out of a JSON message stream.

Cargo does not put a `harness = false` bench target where a path can be guessed. On the toolchain
this was written against it lands under `<target-dir>/release/build/<pkg>/<hash>/out/<name>-<hash>`,
which is not the `release/deps/` a reader expects and is not a layout any script should encode: the
one contract cargo does keep is the `executable` field of the `compiler-artifact` message, so that
is what is read.

Usage:
    exe_path.py <build.json> <target-name>
"""

import json
import sys


def main():
  if len(sys.argv) != 3:
    sys.exit("usage: exe_path.py <build.json> <target-name>")
  stream, wanted = sys.argv[1], sys.argv[2]

  found = []
  with open(stream, encoding="utf-8") as handle:
    for line in handle:
      line = line.strip()
      if not line.startswith("{"):
        continue
      try:
        message = json.loads(line)
      except json.JSONDecodeError:
        continue
      if message.get("reason") != "compiler-artifact":
        continue
      if not message.get("executable"):
        continue
      if message.get("target", {}).get("name") == wanted:
        found.append(message["executable"])

  if not found:
    sys.exit(
      f"::error::no executable for target '{wanted}' in {stream}. A `required-features` entry that "
      "is not satisfied makes cargo SKIP a target silently, so an empty stream here usually means "
      "the build did not fail — it built nothing."
    )
  # The last one wins: a rebuild can emit the artifact more than once, and the final message is
  # the one describing the file that is on disk now.
  print(found[-1])


if __name__ == "__main__":
  main()
