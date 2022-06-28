%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(k3_node_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    
    
    application:load(k3_node),
    {ok,DeploymentNameAtom}=application:get_env(k3_node,deployment_name),
    DeploymentName=atom_to_list(DeploymentNameAtom),
    init_etcd(),
    
    
    
    ok=rpc:call(node(),application,set_env,[[{k3_node,[{deployment_name,DeploymentName}]}]],5000),
    ok=application:start(k3_node),
  
    io:format(" sd_server:all() ~p~n",[ sd_server:all()]),
    io:format(" sd_server:get(k3_node) ~p~n",[ sd_server:get(k3_node)]),
    
   

%    init:stop(),
    ok.

    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
	
init_etcd()->
    ok=application:start(config),
    ok=etcd:appl_start([]),
    pong=etcd:ping(), 
    ok=etcd:dynamic_db_init([]),
    ok=db_host_spec:init_table(),
    ok=db_application_spec:init_table(),
    ok=db_deployment_info:init_table(),
    ok=db_deployments:init_table(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
setup()->
  
    % Simulate host
    R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
    R.
