# help.py (help)
# !/usr/bin/env python3
# coding=utf-8
# young.daniel@epa.gov

"""
Numpy gfortran wrapper.

Python with NumPy (Fortran 90 wrapper) for the Hydrologic Evaluation of Landfill
Performance (HELP) Model Software application. HELP is a DOS executable program.

# TODO: Convert current f90 "HELP" tool from Fortran to C http://www.netlib.org/f2c/
# and use a wrapper.

https://www.fortran90.org/src/rosetta.html
https://www.numfys.net/howto/F2PY/
https://www.youtube.com/watch?v=3G8R92Vx-dY
https://bitbucket.org/chris_richardson/ctypes-demo/src/master/
"""

from ctypes import CDLL, POINTER, c_double
import numpy
