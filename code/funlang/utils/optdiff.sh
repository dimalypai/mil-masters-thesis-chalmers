# This script runs flc without optimisations on a file specified as a first
# argument and collects the output to 'no_opt.mil' file.
# Then it runs flc with optimisations on the same file and collects the output
# to 'opt.mil' file.
# Then it launches a diff program.
# Finally it deletes all created files.
dist/build/flc/flc $1 > /tmp/no_opt.mil
dist/build/flc/flc $1 -O > /tmp/opt.mil
/Applications/p4merge.app/Contents/MacOS/p4merge /tmp/no_opt.mil /tmp/opt.mil
rm /tmp/no_opt.mil /tmp/opt.mil
