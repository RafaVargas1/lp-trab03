% Permite definir predicados não contíguos
:- discontiguous start_game/0.

% Definindo um predicado dinâmico para armazenar o estado do tabuleiro
:- dynamic board/1.

% Tabuleiro inicial
init_board([
    ['B','-','B','-','B','-','B','-'],
    ['-','B','-','B','-','B','-','B'],
    ['B','-','B','-','B','-','B','-'],
    ['-','-','-','-','-','-','-','-'],
    ['-','-','-','-','-','-','-','-'],
    ['-','W','-','W','-','W','-','W'],
    ['W','-','W','-','W','-','W','-'],
    ['-','W','-','W','-','W','-','W']
]).

% Inicializando o jogo com mensagem e tabuleiro
start_game :-
    print_initial_msg(),
    init_board(Board),
    assert(board(Board)),  % Armazena o tabuleiro como predicado dinâmico
    print_board(Board).

% Print da mensagem inicial
print_initial_msg :- 
    write('------- Seja bem-vindo(a) ao Jogo de Damas -------\n'),
    write('--------------------------------------------------\n\n'),
    write(' - Cada coordenada é representada pela sequencia Coluna(Letra) Linha(Número)\n'),
    write(' - Exemplo: a1, b5, h8 \n'),
    write(' - Peças brancas são representadas por W e pretas por B \n'),
    write(' - Coordenadas vazias são representadas por - \n\n'),
    write('---------------------- Comandos----------------------\n\n'),
    write(' - mv(CasaOrigem, CasaDestino) \n'),
    write(' - cap(CasaOrigem, [CasaDestino, ...]) \n\n').

% Conversão de letra para índice
letter_to_index(a, 1).
letter_to_index(b, 2).
letter_to_index(c, 3).
letter_to_index(d, 4).
letter_to_index(e, 5).
letter_to_index(f, 6).
letter_to_index(g, 7).
letter_to_index(h, 8).

% Conversão de coordenada para índice
coord_to_index(Coord, X, Y) :-
    atom_chars(Coord, [Letter, NumberChar]),
    letter_to_index(Letter, X),
    number_chars(Number, [NumberChar]),
    Y is 9 - Number.  % Inversão da linha (8->1, 7->2, ..., 1->8)

% Predicado para exibir o tabuleiro
print_board([]).
print_board([Row|Rest]) :-
    print_row(Row), nl,
    print_board(Rest).

print_row([]).
print_row([Cell|Rest]) :-
    write(Cell), write(' '),
    print_row(Rest).

% Função auxiliar para substituir um elemento numa lista
replace_in_list([_|T], 1, X, [X|T]).
replace_in_list([H|T], I, X, [H|R]) :-
    I > 1,
    I2 is I - 1,
    replace_in_list(T, I2, X, R).

% Função auxiliar para mover uma peça no tabuleiro
move_piece(Board, X1, Y1, X2, Y2, NewBoard) :-
    nth1(Y1, Board, Row1),
    replace_in_list(Row1, X1, '-', NewRow1), % Remove peça da origem
    replace_in_list(Board, Y1, NewRow1, TempBoard1),
    nth1(Y2, TempBoard1, Row2),
    nth1(X1, Row1, Piece), % Pega a peça original
    replace_in_list(Row2, X2, Piece, NewRow2), % Move a peça para o destino
    replace_in_list(TempBoard1, Y2, NewRow2, NewBoard).

% Atualiza o tabuleiro globalmente
update_board(NewBoard) :-
    retract(board(_)),  % Remove o tabuleiro antigo
    assert(board(NewBoard)).  % Armazena o novo tabuleiro

% Movimento simples (mv)
mv(Origem, Destino) :-
    board(Board),  % Obtém o tabuleiro atual
    coord_to_index(Origem, X1, Y1),
    coord_to_index(Destino, X2, Y2),
    move_piece(Board, X1, Y1, X2, Y2, NewBoard),
    update_board(NewBoard),  % Atualiza o tabuleiro
    print_board(NewBoard).

cap(Origem, [Destino|Rest]) :-
    mv(Origem, Destino),
    cap(Destino, Rest).

cap(_, []).

start_game :-
    print_initial_msg(),
    init_board(Board),
    assert(board(Board)),  % Armazena o tabuleiro como predicado dinâmico
    print_board(Board).
