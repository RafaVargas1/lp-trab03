% Permite definir predicados não contíguos
:- discontiguous s/0.

% Definindo um predicado dinâmico para armazenar o estado do tabuleiro
:- dynamic board/1.

% Tabuleiro inicial
init_board([
    ['B','-','B','-','B','-','B','-'],  % Linha 8
    ['-','B','-','B','-','B','-','B'],  % Linha 7
    ['B','-','B','-','B','-','B','-'],  % Linha 6
    ['-','-','-','-','-','-','-','-'],  % Linha 5
    ['-','-','-','-','-','-','-','-'],  % Linha 4
    ['-','W','-','W','-','W','-','W'],  % Linha 3
    ['W','-','W','-','W','-','W','-'],  % Linha 2
    ['-','W','-','W','-','W','-','W']   % Linha 1
]).

% Inicializando o jogo com mensagem e tabuleiro
s :-
    retractall(board(_)),               % Remove qualquer estado anterior do tabuleiro
    print_initial_msg(),
    init_board(Board),
    assert(board(Board)),               % Armazena o tabuleiro como predicado dinâmico
    print_board(Board, 8).

% Print da mensagem inicial
print_initial_msg :- 
    write('------- Seja bem-vindo(a) ao Jogo de Damas -------\n'),
    write('--------------------------------------------------\n\n'),
    write(' - Cada coordenada é representada pela sequência Coluna(Letra) Linha(Número)\n'),
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
print_board([], _) :- 
    write('   ________________\n'),
    write('    A B C D E F G H'), nl.

print_board([Row|Rest], N) :-
    write(N), write(' | '),
    print_row(Row), nl,  
    N1 is N - 1,
    print_board(Rest, N1).

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
    nth1(X1, Row1, Piece),           % Pega a peça original
    Piece \= '-',                     % Garante que há uma peça na origem
    nth1(Y2, Board, Row2),
    nth1(X2, Row2, DestCell),
    DestCell = '-',                   % Garante que o destino está vazio

    % Verifica se o movimento é diagonal
    abs(X1 - X2) =:= 1,               % A coluna de destino deve ser uma a mais ou uma a menos
    abs(Y1 - Y2) =:= 1,               % A linha de destino deve ser uma a mais ou uma a menos

    % Validação para movimento de peça branca ('W') ou preta ('B')
    (Piece = 'W' -> Y2 < Y1;          % Peça branca deve mover "para frente" (Y2 < Y1)
     Piece = 'B' -> Y2 > Y1),         % Peça preta deve mover "para trás" (Y2 > Y1)

    % Se passou nas validações, executa o movimento
    replace_in_list(Row1, X1, '-', NewRow1),       % Remove a peça da origem
    replace_in_list(Board, Y1, NewRow1, TempBoard1),
    replace_in_list(Row2, X2, Piece, NewRow2),     % Move a peça para o destino
    replace_in_list(TempBoard1, Y2, NewRow2, NewBoard).

% Atualiza o tabuleiro globalmente
update_board(NewBoard) :-
    retractall(board(_)),             % Remove todos os fatos antigos do tabuleiro
    assert(board(NewBoard)).          % Armazena o novo tabuleiro

% Movimento simples (mv) com corte para evitar múltiplas soluções
mv(Origem, Destino) :-
    board(Board),                      % Obtém o tabuleiro atual
    coord_to_index(Origem, X1, Y1),
    coord_to_index(Destino, X2, Y2),
    (
        move_piece(Board, X1, Y1, X2, Y2, NewBoard) ->    % Tenta mover a peça
        (
            update_board(NewBoard),        % Atualiza o tabuleiro
            print_board(NewBoard, 8),
            !
        )
    ;
        write('Movimento inválido.\n'), !           % Feedback em caso de erro e corte
    ).

% Captura múltipla (cap) com corte para evitar múltiplas soluções
cap(Origem, [Destino|Rest]) :-
    board(Board),
    coord_to_index(Origem, X1, Y1),
    coord_to_index(Destino, X2, Y2),
    (
        capture_move(Board, X1, Y1, X2, Y2, CapX, CapY) ->
        % Se houver um movimento de captura, o executa
        capture_piece(Board, X1, Y1, X2, Y2, CapX, CapY, NewBoard),
        update_board(NewBoard),
        print_board(NewBoard, 8),
        cap(Destino, Rest)  % Verifica se há mais capturas na sequência
    ;
        write('Movimento de captura inválido ou nenhuma captura disponível.\n'), !
    ).

% Verifica se uma captura é possível
% Verifica se uma captura é possível
can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, Piece) :-
    nth1(Y1, Board, Row1),
    nth1(X1, Row1, Piece),               % Encontra a peça na origem
    Piece \= '-',                         % Certifica-se de que não é uma casa vazia
    nth1(Y2, Board, Row2),
    nth1(X2, Row2, '-'),                  % Verifica se o destino é uma casa vazia
    nth1(CapY, Board, CapRow),
    nth1(CapX, CapRow, CapPiece),         % Pega a peça a ser capturada
    CapPiece \= '-',                      % Certifica-se de que a peça a ser capturada não está numa casa vazia
    (Piece == 'B', CapPiece == 'W';      % Se a peça na origem for 'B', a peça capturada deve ser 'W'
     Piece == 'W', CapPiece == 'B').      % Se a peça na origem for 'W', a peça capturada deve ser 'B'


% Realiza a captura de uma peça
% Realiza a captura de uma peça
capture_piece(Board, X1, Y1, X2, Y2, CapX, CapY, NewBoard) :-
    move_capture(Board, X1, Y1, X2, Y2, TempBoard),
    % Remove a peça capturada
    nth1(CapY, TempBoard, CapRow),
    replace_in_list(CapRow, CapX, '-', NewCapRow),
    replace_in_list(TempBoard, CapY, NewCapRow, NewBoard).


% Verifica e realiza um movimento de captura
% Verifica e realiza um movimento de captura
capture_move(Board, X1, Y1, X2, Y2, CapX, CapY) :-
    % Diagonal direita para frente
    X2 is X1 + 2, Y2 is Y1 + 2,
    CapX is X1 + 1, CapY is Y1 + 1,
    can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, _Piece);
    
    % Diagonal esquerda para frente
    X2 is X1 - 2, Y2 is Y1 + 2,
    CapX is X1 - 1, CapY is Y1 + 1,
    can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, _Piece);
    
    % Diagonal direita para trás
    X2 is X1 + 2, Y2 is Y1 - 2,
    CapX is X1 + 1, CapY is Y1 - 1,
    can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, _Piece);
    
    % Diagonal esquerda para trás
    X2 is X1 - 2, Y2 is Y1 - 2,
    CapX is X1 - 1, CapY is Y1 - 1,
    can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, _Piece).

    
    % Movimento personalizado: 1 casa para a esquerda e 2 casas para trás
    X2 is X1 - 1, Y2 is Y1 - 2,
    CapX is X1 - 0, CapY is Y1 - 1,  % Calculando posição intermediária
    can_capture(Board, X1, Y1, X2, Y2, CapX, CapY, _Piece).

% Predicado para mover uma peça durante uma captura (duas casas diagonais)
move_capture(Board, X1, Y1, X2, Y2, NewBoard) :-
    nth1(Y1, Board, Row1),
    nth1(X1, Row1, Piece),           % Pega a peça original
    Piece \= '-',                     % Garante que há uma peça na origem
    nth1(Y2, Board, Row2),
    nth1(X2, Row2, '-'),             % Garante que o destino está vazio
    
    % Verifica se o movimento é de duas casas diagonais
    DX is X2 - X1,
    DY is Y2 - Y1,
    abs(DX) =:= 2,
    abs(DY) =:= 2,
    
    % Executa o movimento: remove a peça da origem e coloca no destino
    replace_in_list(Row1, X1, '-', NewRow1),
    replace_in_list(Board, Y1, NewRow1, TempBoard1),
    replace_in_list(Row2, X2, Piece, NewRow2),
    replace_in_list(TempBoard1, Y2, NewRow2, NewBoard).