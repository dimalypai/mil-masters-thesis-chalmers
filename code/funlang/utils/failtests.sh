# This script runs testing with cabal and collects the output to 'test_output' file.
# Then in greps expected and actual results to 'expected' and 'got' files respectively.
# Then it expands newlines.
# Then it launches a diff program.
# Finally it deletes all created files.
cabal test > /tmp/test_output
grep expected /tmp/test_output > /tmp/expected
grep got /tmp/test_output > /tmp/got
echo -e "$(cat /tmp/expected)" > /tmp/expected
echo -e "$(cat /tmp/got)" > /tmp/got
/Applications/p4merge.app/Contents/MacOS/p4merge /tmp/expected /tmp/got
rm /tmp/test_output /tmp/expected /tmp/got
