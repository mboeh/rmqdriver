default:
	LDFLAGS=-L/usr/local/opt/readline/lib CPPFLAGS=-I/usr/local/opt/readline/include stack build --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib
