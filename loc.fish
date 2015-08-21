function loc
	cloc puckCore/src/main
    cloc puckJava/src/main
	wc -l (find puckJava/src/main -regextype "posix-extended" -iregex '.*\.(jrag|jadd)')
    cloc puckCore/src/main puckJava/src/main
end
