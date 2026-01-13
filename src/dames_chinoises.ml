(*~~~~~~~~~~~~~~~~~~~~Projet Les Dames Chisnoises~~~~~~~~~~~~~~~~~~~~~~
DESFONDS Antoine | LAUSSAC Guillaume | FATHALLAH Mariam | DIALLO Alpha
~~~~~~~~~~~~~~~~~~~~~~Groupe INF01 Quadrinome D~~~~~~~~~~~~~~~~~~~~~~~~

Projet Première partie (Q1 à Q9)*)

type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

(*Q1

1)Le camp du sud.
2)Le camp du nord.
3)Le camp du nord-ouest.
4)La case la plus au nord.
5)La case en haut à gauche du camp sud.
6)Tout le plateau sauf les camps sud, nord-ouest et nord-est.
*)

(* Q2 *)

let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
  (est_case(i, j, k)) && ((i >= -2*dim) && (i <= 2*dim) && (j >= -dim) && (j <= dim) && (k >= -dim) && (k <= dim))
;;(*val est_dans_losange : case -> dimension -> bool = <fun>*)

assert(est_dans_losange(6,-3,-3)(3) = true);;(*- : unit = ()*)
assert(est_dans_losange(0,0,0)(3) = true);;(*- : unit = ()*)
assert(est_dans_losange(3,-6,3)(3) = false);;(*- : unit = ()*)
assert(est_dans_losange(-3,6,-3)(3) = false);;(*- : unit = ()*)
assert(est_dans_losange(12,6,-3)(3) = false);;(*- : unit = ()*)

(* Q3 *)

let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
  (est_case(i, j, k)) && ((i >= -dim && j >= -dim && k >= -dim)||(i <= dim && j <= dim && k <= dim))
;;

assert(est_dans_etoile(12,6,-3)(3) = false);;(*- : unit = ()*)
assert(est_dans_etoile(3,6,-3)(3) = false);;(*- : unit = ()*)
assert(est_dans_etoile(-4,0,4)(3) = false);;(*- : unit = ()*)
assert(est_dans_etoile(-6,3,3)(3) = true);;(*- : unit = ()*)
assert(est_dans_etoile(1,-1,0)(3) = true);;(*- : unit = ()*)

(* Q4 *)

let rec tourner_case (m:int) ((i,j,k):case):case =
  match m with
  |0 -> (i, j, k)
  |1 -> (-k,-i,-j) 
  |2 -> (j,k,i) 
  |3 -> (-i,-j,-k)
  |4 -> (k, i, j)
  |5 -> (-j, -k, -i)
  |_ -> (tourner_case(m-6)(i, j, k))(*si m>=6 alors on aura fait un tour complet du plateau, et les cases correspondent à m-6*)
;;

assert(tourner_case(1)(-3,-3,6) = (-6,3,3));;(*- : unit = ()*)
assert(tourner_case(7)(-3,-3,6) = (-6,3,3));;(*- : unit = ()*)
assert(tourner_case(2)(6,-3,-3) = (-3,-3,6));;(*- : unit = ()*)
assert(tourner_case(14)(6,-3,-3) = (-3,-3,6));;(*- : unit = ()*)

(* Q5 *)

let translate (c:case) (v:vecteur):case =
  let (i, j ,k) = c and (v1, v2, v3) = v in
  (i+v1, j+v2, k+v3) ;; (*val translate : case -> vecteur -> case = <fun>*)

assert(translate(-4,1,3)(0,2,-2) = (-4,3,1));; (*- : unit = ()*)
assert(translate(-4,3,1)(1,0,-1) = (-3,3,0));; (*- : unit = ()*)

(* Q6 *)

let diff_case (c1:case) (c2:case):vecteur =
  let (i,j,k) = c1 and (l,m,n) = c2 in
  (i-l,j-m,k-n) ;; (*val diff_case : case -> case -> vecteur = <fun>*)

assert(diff_case (0,0,0) (0,-2,2) = (0,2,-2));; (*- : unit = ()*)
assert(diff_case (-6,3,3) (0,-1,1) = (-6,4,2));; (*- : unit = ()*)

(* Q7 *)

let sont_cases_voisines ((i,j,k):case)((x,y,z):case):bool =
  if (x == i+1) then
    if ((y == j-1)&&(z == k)) || ((z == k-1)&&(y == j)) then true else false
  else if y == j+1 then
    if ((x == i-1)&&(z == k)) || ((z == k-1)&&(x == i)) then true else false
  else if z == k+1 then
    if ((x == i-1)&&(y == j)) || ((y == j-1)&&(x == i)) then true else false
  else false
;; (*val sont_cases_voisines : case -> case -> bool = <fun>*)

assert(sont_cases_voisines (-6,3,3) (-5,3,2) = true);; (* - : unit = () *)
assert(sont_cases_voisines (-6,3,3) (-5,2,3) = true);; (* - : unit = () *)
assert(sont_cases_voisines (1,0,-1) (2,0,-2) = true);; (* - : unit = () *)
assert(sont_cases_voisines (1,0,-1) (1,2,-3) = false);; (* - : unit = () *)

(* Q8 *)

let calcul_pivot (c1:case) (c2:case) : case option = 
  let (i,j,k) = c1 and (x,y,z) = c2 in
  if (i == x) || (j == y) || (k == z) then
    let (a,b,c) = (i-x, j-y, k-z) in
    if (a mod 2 == 0) && (b mod 2 == 0) && (c mod 2 == 0) then
      let a = a/2 and b = b/2 and c = c/2 in
      Some((i-a, j-b, k-c))
    else None
  else None;;(*val calcul_pivot : case -> case -> case option = <fun>*)

calcul_pivot (0,-2,2) (0,2,-2);;(*- : case option = Some (0, 0, 0)*)
calcul_pivot(6,-3,-3)(-2,5,-3);;(*- : case option = Some (2, 1, -3)*)

(* Q9 *)



let vec_et_dist (c1:case) (c2:case) : vecteur*int =
  if c1 = c2 then ((0, 0, 0), 0)
  else
    let (i,j,k) = c1 and (x,y,z) = c2 in
    let (a,b,c) = ((if i=x then 0 else i+x),(if j=y then 0 else j+y),(if k=z then 0 else k+z)) in
    let (a2,b2,c2) =
      match (a,b,c) with
      |(0,b,c) -> if (j<y) then (0,1,-1) else (0,-1,1)
      |(a,0,c) -> if (i<x) then (1,0,-1) else (-1,0,1)
      |(a,b,0) -> if (i<x) then (1,-1,0) else (-1,1,0)
      |(_,_,_) -> failwith "Erreur programme. Voir fonction vec_et_dist"(*renvoie un message d'erreur car cas impossible*)
    in
    let v:vecteur = (a2, b2, c2) and d:int = (if (i<>x) then (if (x-i)>=0 then (x-i) else -1*(x-i)) else (if (y-j)>=0 then (y-j) else -1*(y-j))) in
    (v, d)
;;

assert((vec_et_dist(0, 2, -2)(0, 0, 0))=((0,-1,1),2));;(* - : unit = () *)
assert((vec_et_dist(0, 0, 0)(0, 2, -2))=((0,1,-1),2));;(* - : unit = () *)
assert((vec_et_dist(-3, 2, 1)(-3, 2, 1))=((0,0,0),0));;(* - : unit = () *)
assert((vec_et_dist(-4,1,3)(3,1,-4))=((1,0,-1),7));;(* - : unit = () *)
assert((vec_et_dist(3,1,-4)(-4,1,3))=((-1,0,1),7));;(* - : unit = () *)
assert((vec_et_dist(-5,3,2)(2,-4,2))=((1,-1,0),7));;(* - : unit = () *)
assert((vec_et_dist(2,-4,2)(-5,3,2))=((-1,1,0),7));;(* - : unit = () *)
assert((vec_et_dist(2,-3,1)(2,-1,-1))=((0,1,-1),2));;(* - : unit = () *)
assert((vec_et_dist(2,-1,-1)(2,-3,1))=((0,-1,1),2));;(* - : unit = () *)
assert((vec_et_dist(0,0,0)(3,0,-3))=((1,0,-1),3));;(* - : unit = () *)

(*Projet Deuxième Partie*)

(*Q10*)

let tourner_liste (l:'a list):'a list =
  match l with
  |[]->[]
  |x::fin->fin@[x]
;;

assert((tourner_liste([Vert;Jaune;Rouge]))=[Jaune;Rouge;Vert]);;(*- : unit = ()*)

let rec der_liste (l:'a list):'a =
  match l with
  |a::[]->a
  |l->(der_liste (List.tl l))
;;

assert((der_liste([Vert;Jaune;Rouge]))=Rouge);;(*- : unit = ()*)

(*Q11*)

let rec remplir_segment (m:int)(c:case):case list =
  let (i,j,k) = c in
  if m<>0 then (i,j,k)::(remplir_segment(m-1)(i, j+1, k-1))
  else []
;;

(*Q12*)

let rec remplir_triangle_bas (m:int)(c:case):case list =
  let (i,j,k) = c in
  if m<>0 then (remplir_segment m (i,j,k)) @ (remplir_triangle_bas (m-1)(i-1,j+1,k))
  else []
;;

assert((remplir_triangle_bas 3 (-4,1,3))=[(-4,1,3);(-4,2,2);(-4,3,1);(-5,2,3);(-5,3,2);(-6,3,3)]);;(*- : unit = ()*)

(*Q13*)

let rec remplir_triangle_haut (m:int)(c:case):case list =
  let (i,j,k) = c in
  if m<>0 then (remplir_segment m (i,j,k)) @ (remplir_triangle_haut (m-1)(i+1,j,k-1))
  else []
;;

assert((remplir_triangle_haut 3 (-3,4,-1))=[(-3,4,-1);(-3,5,-2);(-3,6,-3);(-2,4,-2);(-2,5,-3);(-1,4,-3)]);;(*- : unit = ()*)

(*Q14*)

let rec colorie (c:couleur)(cl:case list): case_coloree list =
  match cl with
  |[] -> []
  |x::fin -> [(x,c)]@(colorie c fin)
;;

(colorie Vert [(1,1,1);(0,0,0)]);;

(*Q15*)

let tourner_config ((l_case_coul,l_coul,dim):configuration):configuration=
  let l_case_coul_tournee = List.map (fun (ca,co) -> (tourner_case (6/(List.length l_coul)) ca,co)) l_case_coul in 
  let l_coul_tournee = tourner_liste l_coul in 
  (l_case_coul_tournee,l_coul_tournee,dim);;


assert((tourner_config (([((-2,1,1),Vert);((0,-1,1),Jaune)]),([Vert;Jaune]),1))=(([((2,-1,-1),Vert);((0,1,-1),Jaune)]),([Jaune;Vert]),1));;(*- : unit = ()*)
assert((tourner_config (([((-4,1,3),Vert);((-4,3,1),Vert);((-5,2,3),Vert);((-5,3,2),Vert);((-6,3,3),Vert);((2,1,-3),Bleu);]),([Vert;Jaune;Rouge;Bleu;Marron;Noir]),3))=(([((-3,4,-1),Vert);((-1,4,-3),Vert);((-3,5,-2),Vert);((-2,5,-3),Vert);((-3,6,-3),Vert);((3,-2,-1),Bleu);]),([Jaune;Rouge;Bleu;Marron;Noir;Vert]),3));;(*- : unit = ()*)

(*Q16*)

let remplir_init (lj:couleur list)(dim:dimension):configuration =
  let nbj = List.length lj in
  match nbj with
  |1->if dim=1 then ((colorie Vert (remplir_triangle_bas dim (-2,1,1))),[Vert],dim) else if dim=2 then ((colorie Vert (remplir_triangle_bas dim (-3,1,2))),[Vert],dim) else ((colorie Vert (remplir_triangle_bas dim (-4,1,3))),[Vert],dim)
  |2->if dim=1 then ((colorie Jaune (remplir_triangle_haut dim (2,-1,-1)))@(colorie Vert (remplir_triangle_bas dim (-2,1,1))),[Vert;Jaune],dim) else if dim=2 then ((colorie Jaune (remplir_triangle_haut dim (3,-2,-1)))@(colorie Vert (remplir_triangle_bas dim (-3,1,2))),[Vert;Jaune],dim) else ((colorie Jaune (remplir_triangle_haut dim (4,-3,-1)))@(colorie Vert (remplir_triangle_bas dim (-4,1,3))),[Vert;Jaune],dim)
  |3->if dim=1 then ((colorie Jaune (remplir_triangle_haut dim (1,-2,1)))@(colorie Vert (remplir_triangle_bas dim (-2,1,1)))@(colorie Bleu (remplir_triangle_bas dim (1,1,-2))),[Vert;Jaune;Bleu],dim) else if dim=2 then ((colorie Jaune (remplir_triangle_bas dim (2,-4,2)))@(colorie Vert (remplir_triangle_bas dim (-3,1,2)))@(colorie Bleu (remplir_triangle_bas dim (2,1,-3))),[Vert;Jaune;Bleu],dim) else ((colorie Jaune (remplir_triangle_bas dim (3,-6,3)))@(colorie Vert (remplir_triangle_bas dim (-4,1,3)))@(colorie Bleu (remplir_triangle_bas dim (3,1,-4))),[Vert;Jaune;Bleu],dim)
  |6->if dim=1 then ((colorie Jaune (remplir_triangle_haut dim (1,-2,1)))@(colorie Vert (remplir_triangle_bas dim (-2,1,1)))@(colorie Bleu (remplir_triangle_bas dim (1,1,-2)))@(colorie Rouge (remplir_triangle_bas dim (-1,-1,2)))@(colorie Noir (remplir_triangle_bas dim (-1,2,-1)))@(colorie Marron (remplir_triangle_bas dim (2,-1,-1))),[Vert;Rouge;Jaune;Marron;Bleu;Noir],dim) else if dim=2 then ((colorie Jaune (remplir_triangle_bas dim (2,-4,2)))@(colorie Vert (remplir_triangle_bas dim (-3,1,2)))@(colorie Bleu (remplir_triangle_bas dim (2,1,-3)))@(colorie Rouge (remplir_triangle_haut dim (-2,-2,4)))@(colorie Marron (remplir_triangle_haut dim (3,-2,-1)))@(colorie Noir (remplir_triangle_haut dim (-2,3,-1))),[Vert;Rouge;Jaune;Marron;Bleu;Noir],dim) else ((colorie Jaune (remplir_triangle_bas dim (3,-6,3)))@(colorie Vert (remplir_triangle_bas dim (-4,1,3)))@(colorie Bleu (remplir_triangle_bas dim (3,1,-4)))@(colorie Rouge (remplir_triangle_haut dim (-3,-3,6)))@(colorie Marron (remplir_triangle_haut dim (4,-3,-1)))@(colorie Noir (remplir_triangle_haut dim (-3,4,-1))),[Vert;Rouge;Jaune;Marron;Bleu;Noir],dim)
  |_->([],lj,dim)(*plateau vide si nombre de joueurs invalide*)
;;

(*Q17*)

let rec associe (a:'a)(l:('a*'b) list)(b:'b):'b =
  match l with
  |[]->b
  |(a2,b2)::fin->if a2=a then b2 else (associe a fin b)
;;

assert((associe (0,0,0) [((-1,0,1),Jaune);((0,0,0),Vert)] Libre)=Vert);;(*- : unit = ()*)
assert((associe (1,-1,0) [((-1,0,1),Jaune);((0,0,0),Vert)] Libre)=Libre);;(*- : unit = ()*)

let quelle_couleur (c:case)(conf:configuration):couleur =
  let (x,y,z)=conf in
  (associe c x Libre)
;;

assert((quelle_couleur (0,0,0) (([((3,-1,-2),Vert);((-1,0,1),Jaune);((2,1,-3),Vert)]),[Vert;Jaune],6))=Libre);;(*- : unit = ()*)
assert((quelle_couleur (2,2,-4) (([((3,-1,-2),Vert);((-1,0,1),Jaune);((2,2,-4),Noir);((2,1,-3),Vert)]),[Vert;Jaune;Noir],6))=Noir);;(*- : unit = ()*)

(*Q18*)

let rec supliste (cl:case_coloree list)(c:case):case_coloree list=
  match cl with
    |[]->[](*si la case à supprimé n'est pas dans la configuration alors la configuration est renvoyée sans modifications*)
    |x::fin->let (case,coul) = x in if case=c then fin else [x]@(supliste fin c)
;;

let supprime_dans_config (conf:configuration)(c:case):configuration =
  let (casecol,listecol,dim) = conf in
    ((supliste casecol c),listecol,dim)
;;

assert((supprime_dans_config (([((-4,1,3),Vert);((-4,3,1),Vert);((-5,2,3),Vert);((-5,3,2),Vert);((-6,3,3),Vert);((2,1,-3),Bleu);]),([Vert;Jaune;Rouge;Bleu;Marron;Noir]),3) (0,0,0))=(([((-4,1,3),Vert);((-4,3,1),Vert);((-5,2,3),Vert);((-5,3,2),Vert);((-6,3,3),Vert);((2,1,-3),Bleu);]),([Vert;Jaune;Rouge;Bleu;Marron;Noir]),3));;(*- : unit = ()*)
assert((supprime_dans_config (([((-4,1,3),Vert);((-4,3,1),Vert);((-5,2,3),Vert);((-5,3,2),Vert);((-6,3,3),Vert);((2,1,-3),Bleu);]),([Vert;Jaune;Rouge;Bleu;Marron;Noir]),3) (-5,3,2))=(([((-4,1,3),Vert);((-4,3,1),Vert);((-5,2,3),Vert);((-6,3,3),Vert);((2,1,-3),Bleu);]),([Vert;Jaune;Rouge;Bleu;Marron;Noir]),3));;(*- : unit = ()*)

(*Q19*)
(*
let est_coup_valide (conf:configuration)(cp:coup):bool =
  let (Du(c1,c2)) = cp and (ccl, cl, dim)=conf in
    (sont_cases_voisines c1 c2)&&((quelle_couleur c1 conf)=(List.hd cl))&&((quelle_couleur c2 conf)=Libre)&&(est_dans_losange c2 dim)
;;
*)
(*
assert((est_coup_valide (remplir_init [Code "Bob";Code "Kan";Code "Ato";Code "yak";Code "mic";Code "Lou"] 3) (Du((-4,1,3),(-3,1,2))))=true);;(*- : unit = ()*)
assert((est_coup_valide (([((-1,0,1),Vert);((0,0,0),Noir)]),[Vert;Noir],3) (Du((-1,0,1),(0,0,0))))=false);;(*- : unit = ()*)
assert((est_coup_valide (([((-1,0,1),Vert);((0,0,0),Noir)]),[Noir;Vert],3) (Du((-1,0,1),(0,0,0))))=false);;(*- : unit = ()*)
assert((est_coup_valide (([((-1,0,1),Vert);((0,0,0),Noir)]),[Noir;Vert],3) (Du((-1,0,1),(0,-1,1))))=false);;(*- : unit = ()*)
assert((est_coup_valide (([((-1,0,1),Vert);((0,0,0),Noir)]),[Vert;Noir],3) (Du((-1,0,1),(0,-1,1))))=true);;(*- : unit = ()*)
*)

(*Q20*)
(*
let appliquer_coup (conf:configuration)(cp:coup):configuration =
  let Du(c1,c2) = cp in
    let coul=(quelle_couleur c1 conf) in
      let (ccl, cl, dim) = (supprime_dans_config conf c1) in
        (ccl@[(c2, coul)], cl, dim)
;;
*)


(*affiche (appliquer_coup (remplir_init [Code "Bob";Code "Kan";Code "Ato";Code "yak";Code "mic";Code "Lou"] 3) (Du((-4,1,3),(-3,1,2))));;(*- : unit = ()*)
assert((appliquer_coup ([(0,0,0),Vert], [Vert;Jaune], 3) (Du((0,0,0),(-1,0,1))))=([(-1,0,1),Vert], [Vert;Jaune], 3));;(*- : unit = ()*)
*)

(*Q21*)
(*
let mettre_a_jour_configuration (conf:configuration)(cp:coup):configuration =
  if (est_coup_valide conf cp) then (tourner_config (appliquer_coup conf cp)) else failwith "Ce coup n'est pas valide, rejouez."
;;
*)


(*affiche (mettre_a_jour_configuration (remplir_init [Code "Bob";Code "Kan";Code "Ato";Code "yak";Code "mic";Code "Lou"] 3) (Du((-4,1,3),(-3,1,2))));;(*- : unit = ()*)
assert((mettre_a_jour_configuration ([(0,0,0),Vert], [Vert;Jaune], 3) (Du((0,0,0),(-1,0,1))))=([(1,0,-1),Vert], [Jaune;Vert], 3));;(*- : unit = ()*)
(mettre_a_jour_configuration ([(0,0,0),Vert], [Vert;Jaune], 3) (Du((0,0,0),(0,0,0))));;(*Exception: (Failure "Ce coup n'est pas valide, rejouez.")*)
*)

(*Q22*)

let rec est_libre_seg_rec (c1:case)(c2:case)(tr:vecteur)(nb:int)(conf:configuration):bool =
  if nb<>1 then
    let (i,j,k) = c1 and (v1,v2,v3) = tr in
    ((quelle_couleur (i+v1,j+v2,k+v3) conf) = Libre) && (est_libre_seg_rec (i+v1,j+v2,k+v3) c2 tr (nb-1) conf)
  else true
;;

let est_libre_seg (c1:case)(c2:case)(conf:configuration):bool =
  let (tr,nb) = vec_et_dist c1 c2 in
  (est_libre_seg_rec c1 c2 tr nb conf)
;;(*verifie que aucune case n'est occupé entre c1 et c2 mais ne vérifie pas l'occupation de c1 et c2*)

assert((est_libre_seg (0,0,0)(3,0,-3)([((-1,1,0),Jaune);((-4,3,1),Vert)],[Jaune;Vert],3))=true);;(*- : unit = ()*)
assert((est_libre_seg (0,0,0)(3,0,-3)([((-1,1,0),Jaune);((-4,3,1),Vert);((0,0,0),Vert)],[Jaune;Vert],3))=true);;(*- : unit = ()*)
assert((est_libre_seg (0,0,0)(3,0,-3)([((-1,1,0),Jaune);((-4,3,1),Vert);((0,0,0),Vert);((3,0,-3),Vert)],[Jaune;Vert],3))=true);;(*- : unit = ()*)
assert((est_libre_seg (0,0,0)(3,0,-3)([((-1,1,0),Jaune);((-4,3,1),Vert);((0,0,0),Vert);((3,0,-3),Vert);((2,0,-2),Vert)],[Jaune;Vert],3))=false);;(*- : unit = ()*)
assert((est_libre_seg (0,0,0)(3,0,-3)([((-1,1,0),Jaune);((-4,3,1),Vert);((3,0,-3),Vert);((2,0,-2),Vert)],[Jaune;Vert],3))=false);;(*- : unit = ()*)


(*Q23*)

let est_saut (c1:case)(c2:case)(conf:configuration):bool =
  let cint = (calcul_pivot c1 c2) and (ccl, cl, dim) = conf in
  match cint with
  |None->false
  |Some(c)->(est_libre_seg c1 c conf)&&(est_libre_seg c c2 conf)&&((quelle_couleur c1 conf)=(List.hd cl))&&((quelle_couleur c2 conf)=Libre)&&((quelle_couleur c conf)<>Libre)
;;


assert((est_saut (0,-3,3) (4,-3,-1) ([((2,-3,1),Vert);((0,-3,3),Vert);((0,2,-2),Bleu)], [Vert;Bleu], 3))=true);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((2,-3,1),Vert);((0,2,-2),Bleu)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((0,-3,3),Vert);((0,2,-2),Bleu)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((2,-3,1),Vert);((0,-3,3),Bleu);((0,2,-2),Bleu)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((2,-3,1),Vert);((0,-3,3),Vert);((0,2,-2),Bleu);((4,-3,-1),Vert)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((3,-3,0),Vert);((0,-3,3),Vert);((0,2,-2),Bleu)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (4,-3,-1) ([((2,-3,1),Vert);((0,-3,3),Vert);((0,2,-2),Bleu)], [Bleu;Vert], 3))=false);;(*- : unit = ()*)
assert((est_saut (0,-3,3) (3,-3,0) ([((2,-3,1),Vert);((0,-3,3),Vert);((0,2,-2),Bleu)], [Vert;Bleu], 3))=false);;(*- : unit = ()*)

(*Q24*)

let rec parcours_cases (c:case)(liste:case list)(conf:configuration):bool =
  let (ccl, cl, dim) = conf in
  match liste with
  |[]->true
  |x::fin->(est_dans_etoile x dim)&&(est_saut c x conf)&&(parcours_cases x fin ((ccl@[(x,(quelle_couleur c conf))]), cl, dim))
;;

let est_saut_multiple (cl:case list)(conf:configuration):bool =
  let c = List.hd cl and liste = List.tl cl and (ccl, clj, dim) = conf in
  (est_dans_losange (der_liste cl) dim)&&(parcours_cases c liste conf)
;;

assert((est_saut_multiple [(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)] ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Vert;Jaune;Bleu],3))=true);;(*- : unit = ()*)
assert((est_saut_multiple [(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)] ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Jaune;Vert;Bleu],3))=false);;(*- : unit = ()*)
assert((est_saut_multiple [(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)] ([((-3,1,2),Vert);((-1,-1,2),Bleu);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Jaune;Vert;Bleu],3))=false);;(*- : unit = ()*)
assert((est_saut_multiple [(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)] ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Vert;Jaune;Bleu],3))=false);;(*- : unit = ()*)
assert((est_saut_multiple [(-3,1,2);(1,-3,2)] ([((-3,1,2),Vert);((-1,-1,2),Bleu)],[Vert;Jaune;Bleu],3))=true);;(*- : unit = ()*)
assert((est_saut_multiple [(-2,-1,3);(2,-5,3)] ([((0,-3,3),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3))=false);;(*- : unit = ()*)
assert((est_saut_multiple [(-2,-1,3);(2,-5,3);(2,1,-3)] ([((0,-3,3),Bleu);((-2,-1,3),Jaune);((2,-2,0),Jaune)],[Jaune;Bleu],3))=true);;(*- : unit = ()*)


(*Q25*)
(*FAIT (2)*)

let est_coup_valide2 (conf:configuration)(cp:coup):bool =
  match cp with
  |(Du(c1,c2))->let (ccl, cl, dim)=conf in (sont_cases_voisines c1 c2)&&((quelle_couleur c1 conf)=(List.hd cl))&&((quelle_couleur c2 conf)=Libre)&&(est_dans_losange c2 dim)
  |Sm(cl)->(est_saut_multiple cl conf)
;;

let appliquer_coup2 (conf:configuration)(cp:coup):configuration =
  match cp with
  |Du(c1,c2)->let coul=(quelle_couleur c1 conf) and (ccl, cl, dim) = (supprime_dans_config conf c1) in (ccl@[(c2, coul)], cl, dim)
  |Sm(clj)->let coul=(quelle_couleur (List.hd clj) conf) and (ccl, cl, dim) = (supprime_dans_config conf (List.hd clj)) in (ccl@[((der_liste clj), coul)], cl, dim)
;;

let mettre_a_jour_configuration2 (conf:configuration)(cp:coup):configuration =
  if (est_coup_valide2 conf cp) then (tourner_config (appliquer_coup2 conf cp)) else failwith "Ce coup n'est pas valide, rejouez."
;;
(*serie de tests:
affiche (mettre_a_jour_configuration2 ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Vert;Jaune;Bleu],3) (Sm([(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)])));;
affiche (mettre_a_jour_configuration2 ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Bleu;Vert;Jaune],3) (Sm([(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)])));;
affiche (mettre_a_jour_configuration2 (mettre_a_jour_configuration2 ([((-3,1,2),Vert);((-1,-1,2),Bleu);((2,-3,1),Jaune);((0,0,0),Vert);((-2,3,-1),Bleu);((2,0,-2),Bleu)],[Vert;Jaune;Bleu],3) (Sm([(-3,1,2);(1,-3,2);(3,-3,0);(-3,3,0);(-1,3,-2);(5,-3,-2)]))) (Du((-3,1,2),(-2,1,1))));;
affiche (mettre_a_jour_configuration2 ([((0,-3,3),Bleu);((2,-2,0),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3) (Sm([(-2,-1,3);(2,-5,3);(2,1,-3)])));;
affiche (mettre_a_jour_configuration2 ([((0,-3,3),Bleu);((2,-2,0),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3) (Sm([(-2,-1,3);(2,-5,3)])));;(*ne marche pas, normal*)
affiche (mettre_a_jour_configuration2 ([((0,-3,3),Bleu);((2,-2,0),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3) (Sm([(-2,-1,3);(2,-5,3);(2,1,-3);(3,0,-3)])));;(*ne marche pas, normal*)
affiche (mettre_a_jour_configuration2 ([((0,-3,3),Bleu);((2,-2,0),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3) (Sm([(-2,-1,3);(-2,-2,4)])));;(*ne marche pas, normal*)
affiche (mettre_a_jour_configuration2 ([((0,-3,3),Bleu);((2,-2,0),Bleu);((-2,-1,3),Jaune)],[Jaune;Bleu],3) (Du((-2,-1,3),(-2,0,2))));;
*)(*OK*)

(*Q26*)

let score ((l_case_col,l_col,dim):configuration) : int =
  let joueur = List.hd l_col in
  let verif ((case,coul):case_coloree) : bool =
    (quelle_couleur (case) (l_case_col,l_col,dim) = joueur) in 
  (*afin d'avoir une liste des cases du joueur actuel uniquement*)
  let list_joueur = List.filter verif l_case_col in 
  let list_i = List.map (fun ((i,j,k),coul)->i) list_joueur in 
  List.fold_left (+) 0 list_i
;;

assert((score ([((1,-2,1),Bleu);((2,-2,0),Vert);((1,0,-1),Bleu);((6,-3,-3),Bleu);((3,-2,-1),Bleu);((-4,2,2),Vert)],[Vert;Bleu],3))=(-2));;(*- : unit = ()*)

let score_gagnant (dim:dimension) :int =
  let l_case_col = colorie Vert (remplir_triangle_haut (dim) (dim+1,-dim,-1)) and l_col =[Vert]
  in score (l_case_col,l_col,dim);;

assert((score_gagnant 1)=2);;(*- : unit = ()*)
assert((score_gagnant 2)=10);;(*- : unit = ()*)
assert((score_gagnant 3)=28);;(*- : unit = ()*)

(*Q27*)

let gagne ((ccl,cl,dim):configuration):bool =
  ((score (ccl,cl,dim))=(score_gagnant dim))
;;

(*Q28*)

let rec est_partie ((ccl,lj,dim):configuration)(cl:coup list):couleur =
  match cl with
  |[]->if (gagne (ccl,lj,dim)) then (List.hd lj) else Libre
  |cp::fin->(est_partie (mettre_a_jour_configuration2 (ccl,lj,dim) cp) fin)
;;

(*
tests:
(est_partie (remplir_init [Code "Bob";Code "Ant"] 2) [(Du((-3,1,2),(-2,1,1)));(Du((-3,1,2),(-2,1,1)));(Sm([(-3,2,1);(-1,0,1)]));(Sm([(-3,2,1);(-1,0,1)]))])
(est_partie (remplir_init [Code "Ant"] 2) [(Du((-3,1,2),(-2,1,1)));(Sm([(-3,2,1);(-1,0,1)]));(Du((-1,0,1),(0,0,0)))]);;
*)

(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1);;


(*bonus*)
(*variante de est_partie qui ne renvoie pas la couleur du gagnant mais affiche le dernier plateau a jour*)
let rec est_partie_affiche ((ccl,lj,dim):configuration)(cl:coup list):unit =
  match cl with
  |[]->(affiche (ccl,lj,dim))
  |cp::fin->(est_partie_affiche (mettre_a_jour_configuration2 (ccl,lj,dim) cp) fin)
;;

(*test:
(est_partie_affiche (remplir_init [Code "Ant"] 2) [(Du((-3,1,2),(-2,1,1)));(Sm([(-3,2,1);(-1,0,1)]));(Du((-1,0,1),(0,0,0)));(Du((-4,2,2),(-3,1,2)));(Sm([(-3,1,2);(-1,1,0);(1,-1,0)]));(Du((-2,1,1),(-2,0,2)));(Sm([(-2,0,2);(2,0,-2)]));(Sm([(0,0,0);(2,-2,0)]));(Du((1,-1,0),(1,-2,1)));(Sm([(1,-2,1);(3,-2,-1)]));(Sm([(2,-2,0);(4,-2,-2)]));(Du((2,0,-2),(3,-1,-2)));]);;   
*)