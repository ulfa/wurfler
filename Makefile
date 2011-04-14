
all: deps release

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

release: compile
	cd rel
	rebar generate force=1
	chmod +x rel/wurflerservice/bin/wurflerservice
	cd ..
