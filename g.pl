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

% Predicado principal: faz uma jogada aleatória com uma peça de uma cor (W ou B)
jogada_aleatoria(Cor) :-
    board(Board),
    findall([X, Y], find_piece(Board, Cor, X, Y), Peças), % Encontra todas as peças da cor
    Peças \= [], % Verifica se há peças da cor no tabuleiro
    tentar_jogada_aleatoria(Cor, Peças).

% Tenta realizar uma jogada aleatória com qualquer peça de uma cor
tentar_jogada_aleatoria(Cor, Peças) :-
    random_select([X1, Y1], Peças, _),  % Seleciona aleatoriamente uma peça
    board(Board),
    get_valid_moves_or_captures(Board, X1, Y1, Movimentos), % Encontra movimentos/capturas válidas
    (
        Movimentos \= [] ->
        random_select([X2, Y2], Movimentos, _), % Seleciona aleatoriamente um movimento
        fazer_jogada(Cor, [X1, Y1], [X2, Y2])    % Executa a jogada
    ;
        % Se não houver movimentos para essa peça, tenta outra
        Peças \= [] ->
        tentar_jogada_aleatoria(Cor, Peças)
    ;
        write('Nenhuma jogada possível para as peças da cor: '), write(Cor), nl
    ).

% Encontra uma peça de uma cor (W ou B) no tabuleiro
find_piece(Board, Cor, X, Y) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Cor). % Procura uma peça da cor (W ou B) na posição X, Y

% Gera todos os movimentos válidos ou capturas para uma peça na posição (X1, Y1)
get_valid_moves_or_captures(Board, X1, Y1, Movimentos) :-
    findall([X2, Y2], 
        (   move_piece(Board, X1, Y1, X2, Y2, _); 
            capture_move(Board, X1, Y1, X2, Y2, _, _)), 
        Movimentos).

% Executa a jogada: primeiro tenta capturar, depois mover
fazer_jogada(Cor, [X1, Y1], [X2, Y2]) :-
    board(Board),
    (   capture_move(Board, X1, Y1, X2, Y2, CapX, CapY) ->
        capture_piece(Board, X1, Y1, X2, Y2, CapX, CapY, NewBoard),
        update_board(NewBoard),
        write('Captura realizada! ('), write(Cor), write(')'), nl,
        print_board(NewBoard, 8)
    ;   move_piece(Board, X1, Y1, X2, Y2, NewBoard) ->
        update_board(NewBoard),
        write('Movimento realizado! ('), write(Cor), write(')'), nl,
        print_board(NewBoard, 8)
    ;   write('Nenhuma jogada válida encontrada para as peças da cor: '), write(Cor), nl
    ).

% Inicia o ciclo de jogadas alternadas
iniciar_jogo :-
    loop_jogadas('W').

% Ciclo de jogadas alternadas
loop_jogadas(CorAtual) :-
    board(Board),
    % Verifica se ainda há peças de ambas as cores no tabuleiro
    findall([X, Y], find_piece(Board, 'W', X, Y), PeçasBrancas),
    findall([X, Y], find_piece(Board, 'B', X, Y), PeçasPretas),
    (
        PeçasBrancas = [] -> 
        write('Peças brancas não têm mais jogadas. Peças pretas venceram!'), nl
    ;
        PeçasPretas = [] -> 
        write('Peças pretas não têm mais jogadas. Peças brancas venceram!'), nl
    ;
        % Caso ambas as cores ainda tenham peças, prossegue com a jogada alternada
        fazer_jogada_aleatoria(CorAtual),
        alternar_cor(CorAtual, ProximaCor),
        loop_jogadas(ProximaCor)
    ).

% Executa uma jogada aleatória para a cor fornecida
fazer_jogada_aleatoria(Cor) :-
    jogada_aleatoria(Cor).

% Alterna a cor atual para a próxima
alternar_cor('W', 'B').
alternar_cor('B', 'W').

% Implementa os modos de jogo "Computador vs Computador" (CvsC) e "Pessoa vs Máquina" (PvM)

% Inicia o jogo com a opção de escolher o modo de jogo
iniciar :-
    s,  % Inicializa o tabuleiro e exibe as instruções
    write('Escolha o modo de jogo:\n'),
    write('1. Computador vs Computador\n'),
    write('2. Pessoa vs Máquina\n'),
    read(Opcao),
    iniciar_jogo(Opcao).

% Ciclo de jogadas para o modo escolhido
iniciar_jogo(1) :-  % Computador vs Computador
    write('Modo Computador vs Computador iniciado!\n'),
    loop_jogadas_automatica('W').

iniciar_jogo(2) :-  % Pessoa vs Máquina
    write('Modo Pessoa vs Máquina iniciado!\n'),
    escolher_cor_jogador.

% Permite que o jogador escolha sua cor no modo Pessoa vs Máquina
escolher_cor_jogador :-
    write('Escolha sua cor:\n'),
    write('1. Branco (W)\n'),
    write('2. Preto (B)\n'),
    read(Opcao),
    (   Opcao = 1 -> loop_jogadas_pessoa_vs_maquina('W');
        Opcao = 2 -> loop_jogadas_pessoa_vs_maquina('B')
    ).

% Loop para o modo Computador vs Computador (CvsC)
loop_jogadas_automatica(CorAtual) :-
    board(Board),
    % Verifica se ainda há peças de ambas as cores no tabuleiro
    findall([X, Y], find_piece(Board, 'W', X, Y), PeçasBrancas),
    findall([X, Y], find_piece(Board, 'B', X, Y), PeçasPretas),
    (
        PeçasBrancas = [] -> 
        write('Peças brancas não têm mais jogadas. Peças pretas venceram!'), nl
    ;
        PeçasPretas = [] -> 
        write('Peças pretas não têm mais jogadas. Peças brancas venceram!'), nl
    ;
        % Caso ambas as cores ainda tenham peças, prossegue com a jogada alternada
        fazer_jogada_aleatoria(CorAtual),
        alternar_cor(CorAtual, ProximaCor),
        loop_jogadas_automatica(ProximaCor)
    ).

% Loop para o modo Pessoa vs Máquina (PvM)
% Ciclo de jogadas alternadas para Pessoa vs Máquina usando os comandos mv e cap
loop_jogadas_pessoa_vs_maquina(CorJogador) :-
    alternar_cor(CorJogador, CorMaquina),
    board(Board),
    % Verifica se ainda há peças de ambas as cores no tabuleiro
    findall([X, Y], find_piece(Board, 'W', X, Y), PeçasBrancas),
    findall([X, Y], find_piece(Board, 'B', X, Y), PeçasPretas),
    (
        PeçasBrancas = [] -> 
        write('Peças brancas não têm mais jogadas. Peças pretas venceram!'), nl
    ;
        PeçasPretas = [] -> 
        write('Peças pretas não têm mais jogadas. Peças brancas venceram!'), nl
    ;
        % Se é a vez do jogador, permite que ele faça a jogada
        write('Sua vez!\n'),
        jogar_pessoa(CorJogador),
        % Após a jogada do jogador, a máquina faz a sua jogada
        jogar_computador(CorMaquina),
        loop_jogadas_pessoa_vs_maquina(CorJogador)
    ).

% Predicado para a jogada do jogador (pessoa), onde o jogador usa mv ou cap
jogar_pessoa(CorJogador) :-
    write('Digite o comando para movimentar (mv(CasaOrigem, CasaDestino)) ou capturar (cap(CasaOrigem, [CasaDestino, ...])):'), nl,
    read(Jogada),  % Lê o comando do jogador
    ( 
      call(Jogada) ->  % Tenta executar o comando lido (mv ou cap)
      write('Jogada executada com sucesso!'), nl
    ; 
      write('Jogada inválida. Tente novamente.'), nl,
      jogar_pessoa(CorJogador)  % Repete a jogada caso seja inválida
    ).

% Predicado para a jogada do computador, que faz uma jogada aleatória
jogar_computador(CorMaquina) :-
    write('Agora é a vez do computador!\n'),
    jogada_aleatoria(CorMaquina),
    write('Jogada do computador concluída.\n').


% Alterna a cor atual para a próxima
alternar_cor('W', 'B').
alternar_cor('B', 'W').
