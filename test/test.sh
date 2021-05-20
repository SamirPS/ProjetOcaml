cd "/home/samir/CompilExampleCandOcmal/Project/test"
for entry in "Unitaires"/*
do
  echo "$entry"
  ../calc "$entry"
done