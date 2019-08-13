# ascitemp_for.py (Temperature)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov

"""
Routine converts an ASCII file to a formatted file contianing daily values.

Each line of output consists of the year, ten data values, and line number.
Available functions:
- Program Scans
- Temperature
- Add data to existing file
- Create new master file
- Write first screen to input file name the user wishes to use as input
- File is assumed to be ASCII file and will be output as formatted file called
    "Data?.tmp". ? = (4=rain 7=temp 13=srad)
- Enter year of the temperature data
"""
