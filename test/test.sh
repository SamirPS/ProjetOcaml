cd "test"
for entry in "Unitaires"/*
do
  echo "$entry"
  ../calc "$entry"
done

cd "svg"
for entry in "../Intégration"/*
do
  
  echo "$entry"
  ../../calc "$entry"
done
