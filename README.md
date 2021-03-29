# Optimisation-with-R

Il seguente progetto è stato ralizzato per la valutazione finale dell' esame di Ottimizzazione del Master in data science for economics, business and finance (Università degli studi di Milano ).

# description 

Il progetto consiste nel realizzare uno script in R per la localizzazione del punto di ottimo.

Il problema in analisi è il seguente : 

In un teatro c'e' un palco di forma rettangolare di dimensioni 20x10 metri, modellizzato
come il rettangolo [0,20]x[0,10].
Ai due angoli del palco di fronte al pubblico (ovvero in (0,0) e in (20,0) ) 
sono posizionati due microfoni fissi.
Un terzo microfono e' mobile e va posizionato in modo da massimizzare la ricezione 
della voce degli attori.
In uno spettacolo recitano due attori, che durante le scene si muovono secondo un
percorso abbastanza ben definito, ma affetto da rumore. 
Gli attori si muovono in 5 posizioni principali durante lo spettacolo.
L'intensita' del suono
ricevuto dai tre microfoni e' inversamente proporzionale alla distanza dagli attori.
In ogni posizione, la voce e' trasmessa dal microfono che rileva l'intensita' maggiore.
Come va posizionato il microfono mobile (la cui posizione e' fissa durante lo spettacolo),
per massimizzare la ricezione della voce di entrambi gli attori?

# procedimento 

In primo luogo si definisce la Loss Function da minimizzare, dopodichè si implementano i seguenti algoritmi per l'identificazione del punto di minimo: 

- random search
- genetic algorithm
- bat algorithm 
