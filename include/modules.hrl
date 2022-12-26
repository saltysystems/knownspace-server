% canonical thrust directions:
% << West, South, East, North >> 
%            - or - 
% << Left, Bottom, Right, Top >> 

-define(DEFAULT_MODULES, #{
    omni => [
        {thrust, [10,10,10,10]},
        {firing_arc, 360},
        {power, 10},
        {energy_capacity, 100},
        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
    ],
    beam => [
        {firing_arc, 90},
        {type, hitscan},
        {power, -2},
        {hitbox, [{-5, -5}, {5, -5}, {0, 5}]}
    ],
    rocket => [
        {power, -3},
        {thrust, [0,50,0,0]}
    ]
}).
