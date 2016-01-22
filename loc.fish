function loc
	wc -l (find puckJrrt/src/main/jrag/puck -regextype "posix-extended" -iregex '.*\.(jrag|jadd)')
    cloc puckCore/src/main puckJava/src/main puckGui/src/main puckJrrt/src/main
end
