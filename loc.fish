function loc
	cd src/main
	cloc .
	wc -l (find . -regextype "posix-extended" -iregex '.*\.(jrag|jadd)')
	cd ../..
end
