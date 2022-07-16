/* Test programs for predicates are written below the all predicates 
Run-

?-[predicates].
?-run_Program().

*/

ibt(empty).
ibt(node(N,L,R)) :- integer(N), ibt(L), ibt(R).

size(empty,0).
size(node(N,empty,empty),1):- integer(N).
size(node(N,LBT,empty),Cnt) :- integer(N),LBT = node(_,_,_), size(LBT,Cnt_l), Cnt is Cnt_l + 1 .
size(node(N,empty,RBT),Cnt) :- integer(N),RBT = node(_,_,_), size(RBT,Cnt_r), Cnt is Cnt_r + 1 .
size(node(N,LBT,RBT),Cnt) :- integer(N), LBT = node(_,_,_), RBT = node(_,_,_), size(LBT,Cnt_l), size(RBT,Cnt_r), Cnt is Cnt_l + Cnt_r + 1.


height(empty,0).
height(node(_,empty,empty),1).
height(node(_,LBT,empty),H) :- LBT = node(_,_,_), height(LBT,Hl), H is Hl + 1 .
height(node(_,empty,RBT),H) :- RBT = node(_,_,_), height(RBT,Hr), H is Hr + 1 .
height(node(_,LBT,RBT),H) :- LBT = node(_,_,_), RBT = node(_,_,_), height(LBT,Hl), height(RBT,Hr), H is 1 + max(Hl, Hr).


preorder(empty,[]).
preorder(node(X,LBT,RBT),[X|List]) :- preorder(LBT,L_list), preorder(RBT,R_list), append(L_list,R_list,List).

inorder(empty,[]).
inorder(node(X,LBT,RBT),List) :- inorder(LBT,L_list), inorder(RBT,R_list), append(L_list,[X|R_list],List).

postorder(empty,[]).
postorder(node(X,LBT,RBT),List) :- inorder(LBT,L_list), inorder(RBT,R_list), append(L_list,R_list,H_list), append(H_list, [X], List).

trPreorder(BT, L) :- pre_tail([BT], L).

pre_tail([empty], []).
pre_tail([node(X, LBT,RBT)| List], L) :- stack(RBT, List, M_list), stack(LBT, M_list, P_list), pre_tail(P_list, N_list), append([X], N_list, L).

trInorder(BT, L) :- in_tail([BT], L).

in_tail([empty], []).
in_tail([node(X, LBT,RBT)| List], L) :- stack(LBT, List, P_list), in_tail(P_list, N_list), stack(RBT, List, O_list), in_tail(O_list, M_list),  
                                        append(N_list, [X], T_list), append(T_list, M_list, L).
                                    
trPostorder(BT, L) :- post_tail([BT], L).

post_tail([empty], []).
post_tail([node(X, LBT,RBT)| List], L) :- stack(LBT, List, P_list), post_tail(P_list, N_list), stack(RBT, List, O_list), post_tail(O_list, M_list),  
                                            append(N_list,  M_list, T_list), append(T_list, [X], L).

stack(empty, O_st, N_st) :- append([], O_st, N_st).
stack(Item, O_st, N_st) :- append([Item], O_st, N_st).

preET(BT, L) :- eulerTour(BT, Ls), preorder(BT, L). 
inET(BT, L) :- eulerTour(BT, Ls), inorder(BT, L).   
postET(BT, L) :- eulerTour(BT, Ls), postorder(BT, L).

eulerTour(empty,[]).
eulerTour(node(X,LBT,RBT),[X|List]) :- eulerTour(LBT,L_list), eulerTour(RBT,R_list), append(L_list,[X|R_list],H_list), append(H_list, [X], List).

toString(empty, "()").
toString(node(N, LBT, RBT), S) :- toString(LBT, SL), toString(RBT, SR), S = [N, SL, SR].

isBalanced(empty).
isBalanced(node(_, LBT, RBT)) :- isBalanced(LBT), isBalanced(RBT), height(LBT, SL), height(RBT, SR), abs(SL - SR) < 2.

isBST(BT) :- inorder(BT, List), list_order(List).

list_order([X]).
list_order([X, Y | Tail]) :- X =< Y, list_order([Y|Tail]).

div(L, A, X, B) :- append(A, [X|B], L), length(A, N), (length(B, N); length(B, M), M is N-1 ).

makeBST([], empty).
makeBST(L, BST) :-  msort(L, Sl), div(Sl, A, X, B), makeBST(A, BST1), makeBST(B, BST2), BST = node(X, BST1, BST2).

lookup(N, BST) :- preorder(BST, List), member(N, List).

insert(N , empty, node(N, empty ,empty)).
insert(N, node(N, LBT, RBT), node(N, LBT, RBT)):-!.
insert(N, node(X, LBT, RBT), node(X, LBTs, RBT)):- X > N,!,insert(N, LBT, LBTs).
insert(N, node(X, LBT, RBT), node(X, LBT, RBTs)):- N > X,!, insert(N, RBT, RBTs). 

/*-------------------------------------------------------------------------------------------------------------------------------------*/

/* For Testing Programs */

run_Program() :- test_size(), test_height(), test_preoder, test_inoder, test_postoder, test_trPreoder, test_trInoder, test_eulerTour.

test_size() :- size(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), S1),
            S1 = 7, size(empty, S2), S2 = 0,  write("Test case for size() Pass"), nl.

test_height() :- height(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), H1),
                H1 = 3, height(empty, H2), H2 = 0, write("Test case for height() Pass"), nl.

isEqualList([], []).
isEqualList([X|L1], [Y|L2]) :- X=Y, eqList(L1,L2).

test_preoder() :- preorder(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), Lt),
                isEqualList(Lt ,[3, -8, -15, 3, 17, 7, 24]), write("Test case for preorder() Pass"), nl.

test_inoder() :- inorder(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), Lt),
                isEqualList(Lt ,[-15, -8, 3, 3, 7, 17, 24]), write("Test case for inorder() Pass"), nl.

test_postoder() :- postorder(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), Lt),
                isEqualList(Lt ,[-15, -8, 3, 7, 17, 24, 3]), write("Test case for postorder() Pass"), nl.

test_trPreoder() :- trPreorder(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), Lt),
                isEqualList(Lt ,[3, -8, -15, 3, 17, 7, 24]), write("Test case for trPreorder() Pass"), nl.

test_trInoder() :- trInorder(node(3, node(-8, node(-15, empty, empty), node(3, empty, empty)), node(17, node(7, empty, empty), node(24, empty, empty))), Lt),
                isEqualList(Lt ,[-15, -8, 3, 3, 7, 17, 24]), write("Test case for trInorder() Pass"), nl.

test_eulerTour() :- eulerTour(node(6, node(4, empty, empty), node(8, empty, empty)),Lt),
                isEqualList(Lt ,[6, 4, 4, 4, 6, 8, 8, 8, 6]), write("Test case for eulerTour() Pass"), nl.
















