% canonical thrust directions:
% << West, South, East, North >>
%            - or -
% << Left, Bottom, Right, Top >>

-define(CELL_SIZE, 32).

-define(DEFAULT_MODULES, #{
    debug_special => [
        {sprite, "default"},
        {firing_arc, 'ARC_90'},
        {damage, 10},
        {thrust, [100, 100, 100, 100]},
        {max_health, 100},
        {mass, 10},
        {torque, 1},
        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
    ],
    beam => [
        {sprite, "beam"},
        {firing_arc, 'ARC_90'},
        {damage, 10},
        {max_health, 100},
        {mass, 1},
        {hitbox, [{5, 5}, {-5, 5}, {0, -5}]}
    ],
    rtg => [
        {sprite, "reactor"},
        {max_health, 100},
        {mass, 2},
        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
    ],
    rocket => [
        {sprite, "engine"},
        {thrust, [0, 100, 0, 0]},
        {max_health, 100},
        {mass, 1},
        {hitbox, [{3, 3}, {-3, 3}, {-5, -5}, {5, -5}]}
    ],
    gyroscope => [
        {sprite, "gyroscope"},
        {mass, 1},
        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]},
        {max_health, 100},
        {torque, 1000}
    ],
    cargo => [
        {sprite, "cargo"},
        {mass, 1},
        {max_health, 100},
        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
    ]
}).

%-define(DEFAULT_MODULES, #{
%    omni => [
%        {sprite, "omni"},
%        {thrust, <<100, 100, 100, 100>>},
%        {action, 'BALLISTIC'},
%        {firing_arc, 'ARC_360'},
%        {power, 10},
%        {energy_capacity, 100},
%        {torque, 1},
%        {mass, 10},
%        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
%    ],
%    rtg => [
%        {sprite, "reactor"},
%        {energy_capacity, 250},
%        {power, 15},
%        {mass, 1},
%        {hitbox, [{-5, -5}, {5, -5}, {5, 5}, {-5, 5}]}
%    ],
%    beam => [
%        {sprite, "beam"},
%        {firing_arc, 'ARC_90'},
%        {action, 'HITSCAN'},
%        {power, -2},
%        {mass, 1},
%        {hitbox, [{5, 5}, {-5, 5}, {0, -5}]}
%    ],
%    gun => [
%        {sprite, "gun"},
%        {firing_arc, 'ARC_22'},
%        {action, 'BALLISTIC'},
%        {power, -1},
%        {mass, 1},
%        {hitbox, [
%            {-5, -5},
%            {-5, -4},
%            {-4, -4},
%            {-4, 2},
%            {-3, 2},
%            {-3, 4},
%            {-4, 4},
%            {-4, 5},
%            {4, 5},
%            {4, 4},
%            {3, 4},
%            {3, 2},
%            {4, 2},
%            {4, -4},
%            {5, -4},
%            {5, -5}
%        ]}
%    ],
%    rocket => [
%        {sprite, "engine"},
%        {power, -3},
%        {thrust, <<0, 100, 0, 0>>},
%        {mass, 1},
%        {hitbox, [{3, 3}, {-3, 3}, {-5, -5}, {5, -5}]}
%    ],
%    gyroscope => [
%        {sprite, "gyroscope"},
%        {mass, 1},
%        {hitbox, [
%            {0, -5},
%            {3, 0},
%            {0, 5},
%            {-3, 0}
%        ]},
%        {power, -2},
%        {torque, 1000}
%    ],
%    cargo => [
%        {sprite, "cargo"},
%        {mass, 1},
%        {hitbox, [
%            {-5, 5},
%            {-4, 5},
%            {-4, -4},
%            {4, -4},
%            {4, 5},
%            {5, 5},
%            {5, -5},
%            {-5, -5}
%        ]}
%    ]
%}).
