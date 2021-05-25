cd "test"
for entry in "Unitaires"/*
do
  echo "$entry"
  ../rendu "$entry"
done

cd "svg"
for entry in "../Intégration"/*
do
  
  echo "$entry"
  ../../rendu "$entry"
done
