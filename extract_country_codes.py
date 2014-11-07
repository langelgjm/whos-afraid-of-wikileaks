################################################################################
# This code is licensed CC BY
#
# Author: Gabriel J. Michael
#
# Postdoctoral Associate and Resident Fellow
# Information Society Project
# Yale Law School
# 127 Wall St
# New Haven, CT 06511
#
# Personal E-mail: gabriel.j.michael@gmail.com
# Institutional E-mail: gabriel.michael@yale.edu
# Website: www.gabrieljmichael.com
# Twitter: @gabrieljmichael
# Github: github.com/langelgjm
#
# This file contains the Python code used to produce an input data file for 
# the article "Who's Afraid of Wikileaks? Missed Opportunities in Political 
# Science Research"
# 
# The input file should be the text of the leaked TPP intellectual property 
# chapter draft, which can be obtained here: https://wikileaks.org/tpp/
#
# This code will overwrite without warning any existing files that have the 
# following names:
# countries.csv
#
# Tested on Python version 2.7.8
#################################################################################

import sys
import re
import string

# This is the filename that leaks_code.R expects
output_filename = "countries.csv"

# Read command line arguments. First argument is the input filename.
args = sys.argv
if len(args) != 2:
    msg = "Usage: python " + args[0] + " <filename>"
    sys.exit(msg)
else:
    filename = args[1]
    try:
        f = open(filename, "r")
        msg = "Successfully opened input file " + filename + "."
        print msg
    except IOError:
        msg = "Could not read input file " + filename + ". Exiting."
        sys.exit(msg)

# Create an empty list to store our results
result_list = []
for line in f:
    # This regex searches for one or more country codes followed by an optional /, 
    # followed by another optional country code. It allows for numerical footnotes 
    # to intervene.
    # Multiple matches on the same line appear as tuples
    result = re.findall(r"(?:(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*/*)+(?:(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*)*", 
                        line)
    # We want a list with one element for each match
    for r in result:
        result_list.append(r)
f.close()

if len(result_list) == 0:
    sys.exit("No matches found. Are you sure you have the correct input file?")

# Open the output file    
try:
    f = open(output_filename, "w")
    msg = "Successfully opened output file " + output_filename + "."
    print msg
except IOError:
    msg = "Could not open output file " + output_filename + ". Exiting."
    sys.exit(msg)

# Translate matches to desired format
for r in result_list:
        # Eliminate footnote numbers
        r = r.translate(None, '0123456789')
        # Replace / with , to convert to CSV
        r = r.translate(string.maketrans('/', ','))
        f.write(r + "\n")
f.close()

print "Done."