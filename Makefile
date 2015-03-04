all:
	erlc -o ./bin/ ./src/*.erl

clean:
	rm ./bin/*.beam
	