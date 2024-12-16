% Move empty space (0) right
move([0, B, C, D, E, F, G, H, J], [B, 0, C, D, E, F, G, H, J]).
move([A, 0, C, D, E, F, G, H, J], [A, C, 0, D, E, F, G, H, J]).
move([A, B, 0, D, E, F, G, H, J], [A, B, F, D, E, 0, G, H, J]).
move([A, B, C, 0, E, F, G, H, J], [A, B, C, E, 0, F, G, H, J]).
move([A, B, C, D, 0, F, G, H, J], [A, B, C, D, F, 0, G, H, J]).
move([A, B, C, D, E, 0, G, H, J], [A, B, C, D, E, J, G, H, 0]).
move([A, B, C, D, E, F, 0, H, J], [A, B, C, D, E, F, H, 0, J]).
move([A, B, C, D, E, F, G, 0, J], [A, B, C, D, E, F, G, J, 0]).

% Move empty space (0) left
move([A, 0, C, D, E, F, G, H, J], [0, A, C, D, E, F, G, H, J]).
move([A, B, 0, D, E, F, G, H, J], [A, 0, B, D, E, F, G, H, J]).
move([A, B, C, D, 0, F, G, H, J], [A, B, C, 0, D, F, G, H, J]).
move([A, B, C, D, E, 0, G, H, J], [A, B, C, D, 0, E, G, H, J]).
move([A, B, C, D, E, F, G, 0, J], [A, B, C, D, E, F, 0, G, J]).
move([A, B, C, D, E, F, G, H, 0], [A, B, C, D, E, F, G, 0, H]).
 
% Move empty space (0) up
move([A, B, C, 0, E, F, G, H, J], [0, B, C, A, E, F, G, H, J]).
move([A, B, C, D, 0, F, G, H, J], [A, 0, C, D, B, F, G, H, J]).
move([A, B, C, D, E, 0, G, H, J], [A, B, 0, D, E, C, G, H, J]).
move([A, B, C, D, E, F, 0, H, J], [A, B, C, 0, E, F, D, H, J]).
move([A, B, C, D, E, F, G, 0, J], [A, B, C, D, 0, F, G, E, J]).
move([A, B, C, D, E, F, G, H, 0], [A, B, C, D, E, 0, G, H, F]).
 
% Move empty space (0) down
move([0, B, C, D, E, F, G, H, J], [D, B, C, 0, E, F, G, H, J]).
move([A, 0, C, D, E, F, G, H, J], [A, E, C, D, 0, F, G, H, J]).
move([A, B, 0, D, E, F, G, H, J], [A, B, F, D, E, 0, G, H, J]).
move([A, B, C, 0, E, F, G, H, J], [A, B, C, G, E, F, 0, H, J]).
move([A, B, C, D, 0, F, G, H, J], [A, B, C, D, H, F, G, 0, J]).
move([A, B, C, D, E, 0, G, H, J], [A, B, C, D, E, J, G, H, 0]).
