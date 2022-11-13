-module(ks_input).

-export([proc_reset/1, insert/3, key_table/0]).

% TODO: gen_server this? for on-the-fly updates to the input table O_o
key_table() ->
    % This table is the master list of all input functions along with the
    % associated function that should be applied when pressed
    [
        {'IMPULSE_FWD', fun(ID, _, Q) -> ks_phys:apply_move(impulse, fwd, ID, Q) end},
        {'IMPULSE_REV', fun(ID, _, Q) -> ks_phys:apply_move(impulse, rev, ID, Q) end},
        {'IMPULSE_LEFT', fun(ID, _, Q) -> ks_phys:apply_move(impulse, left, ID, Q) end},
        {'IMPULSE_RIGHT', fun(ID, _, Q) -> ks_phys:apply_move(impulse, right, ID, Q) end},
        {'ROTATE_LEFT', fun(ID, _, Q) -> ks_phys:apply_move(rotate, left, ID, Q) end},
        {'ROTATE_RIGHT', fun(ID, _, Q) -> ks_phys:apply_move(rotate, right, ID, Q) end},
        {'ACTION_0', fun(ID, C, Q) -> ks_projectile:create_projectile(ID, C, Q) end}
    ].

insert(Input1, ID, World) ->
    Query = ow_ecs:query(World),
    logger:debug("Adding input component to ~p with data ~p", [ID, Input1]),
    % If there's already input, we need to merge it.
    case ow_ecs:try_component(input, ID, Query) of
        false -> 
            % Just add it
            ow_ecs:add_component(input, [Input1], ID, Query);
        Components ->
            Input0 = ow_ecs:get(input, Components),
            Input = [ Input1 | Input0 ], 
            ow_ecs:add_component(input, Input, ID, Query)
    end.

proc_reset(Query) ->
    % Get all entities with input
    E = ow_ecs:match_component(input, Query),
    [ ow_ecs:del_component(input, ID, Query) || {ID, _} <- E ].
