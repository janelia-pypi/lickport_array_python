
# Table of Contents

1.  [Package Information](#org6f1a151)
2.  [Example Usage](#orgba4b75c)
3.  [Data](#org5416024)
    1.  [time](#org30b583e)
    2.  [millis](#org4a74391)
    3.  [lickport<sub>n</sub>](#orge5abd94)
4.  [Installation](#org38d0931)
    1.  [Linux and Mac OS X](#org2648ba1)
    2.  [Windows](#orgb69bcd0)
    3.  [Guix](#org870dc9a)
5.  [Development](#org7dcc38a)
    1.  [Guix](#org7e7bc2e)

-\*- after-save-hook: org-md-export-to-markdown;


<a id="org6f1a151"></a>

# Package Information

-   **Name:** lickport<sub>array</sub><sub>interface</sub><sub>python</sub>
-   **Version:** 1.0.0
-   **License:** BSD 3-Clause License
-   **URL:** <https://github.com/janelia-pypi/lickport_array_interface_python>
-   **Author:** Peter Polidoro
-   **Email:** peter@polidoro.io
    
    This Python package (lickport<sub>array</sub><sub>interface</sub>)


<a id="orgba4b75c"></a>

# Example Usage

    
    from lickport_array_interface import LickportArrayInterface
    dev = LickportArrayInterface() # Try to automatically detect port
    dev = LickportArrayInterface(port='/dev/ttyACM0') # Linux specific port
    dev = LickportArrayInterface(port='/dev/tty.usbmodem262471') # Mac OS X specific port
    dev = LickportArrayInterface(port='COM3') # Windows specific port
    
    data_path_string = '~/lickport_array_data/data_file'
    
    dev.start_acquiring_data()
    dev.start_saving_data(data_path_string)
    dev.stop_saving_data()
    dev.stop_acquiring_data()
    
    dev.controller.dispense_lickport_for_duration(0,200)
    dev.controller.dispense_lickports_for_duration([0,1],200)
    dev.controller.dispense_all_lickports_for_duration(200)
    dev.controller.get_activated_lickports()
    dev.controller.activate_only_lickport(0)
    dev.controller.activate_only_lickports([0,1])
    dev.controller.activate_lickport(0)
    dev.controller.activate_lickports([0,1])
    dev.controller.deactivate_lickport(0)
    dev.controller.deactivate_lickports([0,1])
    dev.controller.activate_all_lickports()
    dev.controller.deactivate_all_lickports()


<a id="org5416024"></a>

# Data

    
    time,millis,lickport_0,lickport_1,lickport_2,lickport_3,lickport_4,lickport_5,lickport_6,lickport_7,lickport_8,lickport_9,lickport_10,lickport_11
    1649700627,12084104,,,,,,,,,,,L,
    1649700628,12085195,L,,,,,,,,,,,
    1649700628,12085749,,,,,,,,,,,,L


<a id="org30b583e"></a>

## time

time in seconds since the epoch

The epoch is the point where the time starts, and is platform dependent. For
Unix, the epoch is January 1, 1970, 00:00:00 (UTC)

The term seconds since the epoch refers to the total number of elapsed seconds
since the epoch, typically excluding leap seconds. Leap seconds are excluded
from this total on all POSIX-compliant platforms.


<a id="org4a74391"></a>

## millis

The number of milliseconds passed since the LickportArrayController board was
powered. This number will overflow (go back to zero), after approximately 50
days.


<a id="orge5abd94"></a>

## lickport<sub>n</sub>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol</th>
<th scope="col" class="org-left">Meaning</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">"L"</td>
<td class="org-left">lickport<sub>n</sub> lick detected</td>
</tr>


<tr>
<td class="org-left">"A"</td>
<td class="org-left">lickport<sub>n</sub> activated</td>
</tr>


<tr>
<td class="org-left">"LA" or "AL"</td>
<td class="org-left">lickport<sub>n</sub> lick detected and activated</td>
</tr>


<tr>
<td class="org-left">""</td>
<td class="org-left">lickport<sub>n</sub> neither lick detected nor activated</td>
</tr>
</tbody>
</table>


<a id="org38d0931"></a>

# Installation

<https://github.com/janelia-pypi/python_setup>


<a id="org2648ba1"></a>

## Linux and Mac OS X

    
    python3 -m venv ~/venvs/lickport_array_interface
    source ~/venvs/lickport_array_interface/bin/activate
    pip install lickport_array_interface


<a id="orgb69bcd0"></a>

## Windows

    
    python3 -m venv C:\venvs\lickport_array_interface
    C:\venvs\lickport_array_interface\Scripts\activate
    pip install lickport_array_interface


<a id="org870dc9a"></a>

## Guix

    
    git clone https://github.com/janelia-pypi/lickport_array_interface_python
    cd lickport_array_interface_python
    guix time-machine -C channels.scm -- shell --pure -f guix.scm python-ipython
    ipython


<a id="org7dcc38a"></a>

# Development


<a id="org7e7bc2e"></a>

## Guix

    
    git clone https://github.com/janelia-pypi/lickport_array_interface_python
    cd lickport_array_interface_python
    guix time-machine -C channels.scm -- shell --pure -D -f guix.scm
    python3 setup.py sdist bdist_wheel
    twine upload dist/*
    git add --all
    git clean -xdf
    exit

