#!/usr/bin/env python3
"""Collect each side's wall_clock binary, symbols, code and instruction counts.

Usage:
    codecheck.py collect <work>
    codecheck.py digest <base prefix> <head prefix>

`collect` writes into `<work>/codecheck/`. Every step is best effort: a failure prints one loud
line and the remaining files are still written.
"""

import gzip
import hashlib
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def loud(message):
  print(f"codecheck: FAILED: {message}", flush=True)


def run_to(cmd, path, gz=False, timeout=None):
  """Run `cmd`, streaming stdout to `path` (gzipped when `gz`). Returns True on success."""
  opener = gzip.open if gz else open
  try:
    with opener(path, "wb") as out:
      proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
      try:
        for chunk in iter(lambda: proc.stdout.read(1 << 20), b""):
          out.write(chunk)
        err = proc.stderr.read()
        code = proc.wait(timeout=timeout)
      except Exception:
        proc.kill()
        raise
    if code != 0:
      loud(f"{' '.join(cmd[:2])} exited {code}: {err.decode(errors='replace')[-300:]}")
      return False
    return True
  except Exception as error:  # noqa: BLE001
    loud(f"{' '.join(cmd[:2])}: {error}")
    return False


def capture(cmd):
  try:
    done = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
    return (done.stdout + done.stderr).strip()
  except Exception as error:  # noqa: BLE001
    return f"unavailable: {error}"


def gzip_file(src):
  with open(src, "rb") as i, gzip.open(src + ".gz", "wb") as o:
    shutil.copyfileobj(i, o)
  os.remove(src)


def side(work, out, name):
  prefix = os.path.join(out, name)
  try:
    exe = subprocess.run(
      [sys.executable, os.path.join(HERE, "exe_path.py"), os.path.join(work, f"{name}.build.json"), "wall_clock"],
      capture_output=True, text=True, check=True,
    ).stdout.strip()
    binary = prefix + ".wall_clock"
    shutil.copyfile(exe, binary)
    os.chmod(binary, 0o755)
  except Exception as error:  # noqa: BLE001
    loud(f"{name}: no binary: {error}")
    return
  digest = hashlib.sha256()
  with open(binary, "rb") as handle:
    for chunk in iter(lambda: handle.read(1 << 20), b""):
      digest.update(chunk)
  with open(prefix + ".sha256", "w") as handle:
    handle.write(f"{digest.hexdigest()}  {name}.wall_clock  {os.path.getsize(binary)}\n")

  run_to(["nm", "--defined-only", "-S", "-n", binary], prefix + ".nm.txt")
  run_to(["objdump", "-d", "--no-show-raw-insn", "-M", "intel", "-w", binary],
         prefix + ".objdump.txt.gz", gz=True)

  raw = prefix + ".callgrind.out"
  if shutil.which("valgrind") is None:
    loud(f"{name}: no valgrind, so no counts")
    return
  cmd = [
    "timeout", "20m", "valgrind", "--tool=callgrind", "--dump-instr=yes", "--cache-sim=yes",
    "--branch-sim=yes", "--separate-recs=1", "--compress-strings=no", "--compress-pos=no",
    "--demangle=no", f"--callgrind-out-file={raw}", binary, "--rounds", "1",
  ]
  done = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
  if done.returncode != 0 or not os.path.exists(raw):
    loud(f"{name}: valgrind exited {done.returncode}: {done.stderr.decode(errors='replace')[-300:]}")
    if not os.path.exists(raw):
      return
  gzip_file(raw)


def collect(work):
  out = os.path.join(work, "codecheck")
  os.makedirs(out, exist_ok=True)
  for name in ("base", "head"):
    try:
      side(work, out, name)
    except Exception as error:  # noqa: BLE001
      loud(f"{name}: {error}")
  lines = [
    "$ rustc -vV", capture(["rustc", "-vV"]),
    "$ lscpu (model name)", "\n".join(l for l in capture(["lscpu"]).splitlines() if "Model name" in l),
    "$ valgrind --version", capture(["valgrind", "--version"]),
    "$ objdump --version", capture(["objdump", "--version"]).splitlines()[0] if capture(["objdump", "--version"]) else "",
  ]
  with open(os.path.join(out, "versions.txt"), "w") as handle:
    handle.write("\n".join(lines) + "\n")


def main():
  if len(sys.argv) >= 3 and sys.argv[1] == "collect":
    collect(sys.argv[2])
  elif len(sys.argv) >= 2 and sys.argv[1] == "digest":
    print("digest: not implemented in this commit")
  else:
    sys.exit("usage: codecheck.py collect <work> | digest <base prefix> <head prefix>")


if __name__ == "__main__":
  main()
