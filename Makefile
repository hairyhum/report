#!/usr/bin/env sh
APP_NAME = report
PWD=`pwd`
ifndef branch
	branch := 'develop'
endif

help:
	@printf "\
	 REBAR: \n\
	dep		- install dependencies \n\
	clean		- clean compiled project and dependencies \n\
	compile		- compile project and dependencies \n\
	generate	- generate reltool release \n\
	\n UTILITY \n\
	rebuild		- clean and compile project\n\
	regenerate	- rebuild and generate releae directory\n\
	reset		- regenerate and restart applicaion (uses softstop)\n\
	reload		- compile and reload erlang code\n\
	restart_app		- restart application services (required if db service was changed)\n\
	\n APPLICATION \n\
	start		- start an applicaion\n\
	stop		- stop an applicaion\n\
	restart		- restart an applicaion\n\
	softstop	- stop applicaion without error if not running\n\
	console		- start applicaion in console mode\n\
	attach		- attach to running applicaionnode\n\
	ping		- ping working applicaion\n\
	status		- show workers status\n\
	\n DIALYZER \n\
	build_plt	- create plt file \n\
	analyze		- analyse project with generated plt file\n\
	"


status:
	escript apps/report/src/show_status.escript

ulimit:
	sh -c 'ulimit -Sd 200000'

dep:
	./rebar get-deps

reset: rebuild generate

hard_reset: reset restart

rebuild: clean compile

clean:
	./rebar clean
	rm -rf apps/report/logs/*
compile:
	./rebar compile
generate: 
	./rebar generate
	touch $(PWD)/log/erl_info.log
	touch $(PWD)/log/erl_debug.log
	touch $(PWD)/log/erl_error.log
	ln -sf $(PWD)/log/erl_* rel/report/log
	chmod 777 rel/$(APP_NAME)/bin/$(APP_NAME)

console: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) console
start: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) start
restart: softstop start
softstop:
	make stop ; true
stop:
	./rel/$(APP_NAME)/bin/$(APP_NAME) stop
ping:
	./rel/$(APP_NAME)/bin/$(APP_NAME) ping
foreground: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) foreground
attach:
	./rel/$(APP_NAME)/bin/$(APP_NAME) attach
test:
	./rebar ct skip_deps=true

reload: compile
	cp apps/$(APP_NAME)/ebin/* rel/$(APP_NAME)/lib/$(APP_NAME)-1/ebin
	cp apps/$(APP_NAME)/include/* rel/$(APP_NAME)/lib/$(APP_NAME)-1/include
	ls apps/$(APP_NAME)/src/*.erl | cut -d. -f1 | cut -d/ -f4 | xargs escript apps/$(APP_NAME)/src/reload_modules.escript 

restart_app: reload
	escript apps/$(APP_NAME)/src/restart_app.escript

reload_services: compile
	find deps -maxdepth 1 -type d | cut -d'/' -f2 | grep -v deps | xargs -L1 sh update_dep.sh
	

build_plt:
	ERL_LIBS=$(PWD)/deps dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc -r deps
analyze: compile
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r apps/$(APP_NAME)/  -I deps --verbose | fgrep -v -f ./dialyzer.ignore-warnings


deploy_staging:
	bundle exec cap deploy target=staging branch=$(branch)

deploy_production:
	bundle exec cap deploy target=master
