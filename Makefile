all:
	rm -rf  *~ */*~ apps/k3_node/src/*.beam test/*.beam erl_cra*;
	rm -rf  k3_node* logs *.pod_dir rebar.lock;
	rm -rf _build test_ebin ebin
	rm -rf deployments temp *_info_specs;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
create:
	rm -rf  *~ */*~ apps/k3_node/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
	git clone https://github.com/joq62/application_info_specs.git;
	git clone https://github.com/joq62/deployment_info_specs.git;
#	Delete and create new k3_node dir to make a clean start
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	erl -pa * -pa ebin -sname k3_node -run k3_node start;
	echo Done
eunit:
	rm -rf  *~ */*~ apps/k3_node/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments host_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	rm -rf  *~ */*~ apps/k3/src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir
	rm -rf deployments *_info_specs;
	rm -rf rebar.lock;
	rm -rf ebin;
#	host_info_specs dir and deployments dir shall be installed once
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	git clone https://github.com/joq62/deployments.git;
	git clone https://github.com/joq62/application_info_specs.git;
	git clone https://github.com/joq62/deployment_info_specs.git;
#	Delete and create new k3_node dir to make a clean start
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
#	testing
	mkdir test_ebin;
	erlc -o test_ebin test/*.erl;
	erl -pa * -pa ebin -pa test_ebin -sname k3_node_test -run k3_node_eunit start -k3_node deployment_name $(deployment_name)
