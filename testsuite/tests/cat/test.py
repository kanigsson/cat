import subprocess

o = subprocess.check_output(["cat", "example"])
o2 = subprocess.check_output(["/usr/bin/cat", "example"])

assert o == o2
