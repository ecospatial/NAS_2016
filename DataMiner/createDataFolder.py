import os
import sys
from pathlib import Path

if len(sys.argv) != 2:
    print('You must pass the data directory as the first argument. E.g. "py createDataFolder.py C:/DATA"')
    exit()

dir = sys.argv[1]

Path(dir + "/CCAP/T0/Change9606").mkdir(parents=True, exist_ok=True)
Path(dir + "/CCAP/T1").mkdir(parents=True, exist_ok=True)
