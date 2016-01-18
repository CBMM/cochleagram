./static.sh
cp ./static/* ./dist/build/cochleagram/cochleagram.jsexe/
cd dist/build/cochleagram/cochleagram.jsexe && git add *.js *.stats *.css *.html && git commit -m "build" && git push origin gh-pages
