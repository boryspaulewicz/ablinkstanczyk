## Koster: T1 był poz, neu lub neg, po 9 samoopisowych przymiotników,
## T2 zawsze neu. Słowa T1 miały długość 4-6 znaków, wybrane w oparciu
## o oceny emocji i familiarności. T2 był wybierany z zestawu 27 słów
## neutralnych 3-6 znaków. Każde ze słów T1 i T2 powtórzyło sie 5
## razy. Jako dystraktory użyto zesta 79 słów mało znanych, 9-18
## znaków. 10 prób treningowych i 135 właściwych (135 / 27 = 5).

## Fajny zestaw słów do zadania pamięciowego lub att blinka jest w
## grant.sonata/pilot/memtrain

library(stringr)
if(interactive())source('~/cs/code/r/tasks/task/task.R')
TASK.NAME <<- 'ablinkstanczyk'

FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 120
MAX.REACTION.TIME = 3000
ISI = 16
NOF.ITEMS = 15
BLOCK.LENGTH = 48
KEYS <<- c(Key.Left, Key.Right)

## WINDOW$set.visible(T)
## WINDOW$set.mouse.cursor.visible(T)

## Wczytujemy wszystkie obrazki
i = new(Image)
load.jpgs = function(folder){
    files = dir(folder)
    pictures = list()
    for(f in files){
        i$load.from.file(paste(folder, f, sep = ''))
        s = new(Sprite)
        tekstura = new(Texture)
        tekstura$create(i$size[1], i$size[2])
        tekstura$load.from.image(i, c(0, 0, i$size))
        s$set.texture(tekstura, F)
        pictures[[length(pictures) + 1]] = c(s, tekstura)
        center(pictures[[length(pictures)]][[1]], WINDOW)
    }
    pictures
}

## T1
black = load.jpgs('./nowe/czarna ramka/')
spiders = load.jpgs('./nowe/pajaki/')
violence = load.jpgs('./nowe/przemoc/')
## T2
green = load.jpgs('./nowe/zielona ramka/')
red = load.jpgs('./nowe/czerwona ramka/')
## Wszystkie w jednej liście do łatwiejszego wybierania
pictures = list(black = black, green = green, red = red, spiders = spiders, violence = violence)

FX = fixation(WINDOW, size = .02)

trial.code = function(trial, t1type = sample(c('black', 'violence', 'spiders'), 1), t1pos = sample(c(4, 7), 1),
    t2type = sample(c('red', 'green'), 1), t2lag = sample(c(2, 5), 1), feedback = 0){
    t1type = as.character(t1type)
    t2type = as.character(t2type)
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    t2pos = t1pos + t2lag
    ## Mieszamy indeksy obrazków
    black.i = sample(1:length(black))
    spiders.i = sample(1:length(spiders))
    violence.i = sample(1:length(violence))
    red.i = sample(1:length(red))
    green.i = sample(1:length(green))
    ## Wszystkie indeksy losowe w jednej liście, żeby było łatwiej wybierać
    indices = list(black = black.i, green = green.i, red = red.i, spiders = spiders.i, violence = violence.i)
    if(trial == 1){
        state = 'press-space'
    }else if((trial %% BLOCK.LENGTH) == 0){
        state = 'break'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Naciśnij spację")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'break' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Krótka przerwa - odpocznij. Aby kontynuować, naciśnij spację")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
                item = 1
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            if(item == t1pos){
                i = pictures[[t1type]][[indices[[t1type]][item]]][[1]]
            }else if(item == t2pos){
                i = pictures[[t2type]][[indices[[t2type]][item]]][[1]]
            }else{
                i = black[[black.i[item]]][[1]]
            }
            WINDOW$draw(i)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) >= PRESENTATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'stim-cleared'
            }
        }, 'stim-cleared' = {
            if((CLOCK$time - stim.cleared) >= ISI){
                if(item <= NOF.ITEMS){
                    item = item + 1
                    state = 'show-stim'
                }else{
                    state = 'show-redgreen'
                }
            }
        }, 'show-redgreen' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("CZERWONA     ZIELONA")
            center(TXT, WINDOW)
            WINDOW$draw(TXT)
            WINDOW$display()
            redgreen.onset = CLOCK$time
            CORRECT.KEY <<- c(red = Key.Left, green = Key.Right)[t2type]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - redgreen.onset) > MAX.REACTION.TIME)){
                if(feedback == 1){
                    feedback.onset = CLOCK$time
                    state = 'feedback'
                }else{
                    state = 'done'
                }
            }
        }, 'feedback' = {
            if((CLOCK$time - feedback.onset) < FEEDBACK.TIME){
                WINDOW$clear(c(.5, .5, .5))
                TXT$set.string(c('Źle', 'Dobrze', 'Za późno')[ifelse(is.null(ACC), 3, ACC + 1)])
                WINDOW$draw(center.win(TXT))
                WINDOW$display()
            }else{
                state = 'done'
            }
        }, 'done' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$display()
            return(list(rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - redgreen.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

gui.show.instruction("W czasie eksperymentu obowiązuje cisza. Proszę wyłączyć telefon komórkowy.
W razie jakichkolwiek wątpliwości proszę nie wołać osoby prowadzącej, tylko podnieść do góry rękę.
Osoba prowadząca podejdzie w dogodnym momencie i postara się udzielić wszelkich wyjaśnień. 
Badanie jest anonimowe.

Za chwilę trzeba będzie wpisać dane osobowe: wiek, płeć oraz pseudonim.
Pseudonim składa się z inicjałów (małymi literami) oraz czterech cyfr:
dnia i miesiąca urodzenia (np.  ms0706).")
gui.user.data('^[a-zA-Z0-9]+$', 'Identyfikator nie może zawierać spacji')

gui.show.instruction("Teraz rozpocznie się zadanie wykrywania koloru ramki. Zadanie to składa się z serii prób, w trakcie których na ekranie komputera prezentowane są szybko, jedno po drugim, różne obrazy. W pewnym momencie prezentowane są obrazy otoczone ramką w kolorze zielonym lub czerwonym.

Zadanie to polega na zaznaczeniu, za pomocą klawiszy strzałek, czy pojawiła się ramka czerwona, czy zielona. Jeżeli pojawiła się ramka CZERWONA, należy nacisnąć klawisz STRZAŁKA W LEWO, a jeżeli ZIELONA, to klawisz STRZAŁKA W PRAWO.

Najpierw rozpocznie się sesja treningowa, składająca się z 10 prób. Celem tej sesji będzie zapoznanie się z działaniem zadania.

Należy reagować możliwie poprawnie.")

run.trials(trial.code, record.session = F, condition = 'default',
           expand.grid(t1type = c('black'), t1pos = c(4, 7),
                       t2type = c('red', 'green'), t2lag = c(2, 5, 8)),
           nof.trials = 10)

run.trials(trial.code, record.session = T, condition = 'default',
           expand.grid(t1type = c('black', 'violence', 'spiders'), t1pos = c(4, 7),
                       t2type = c('red', 'green'), t2lag = c(2, 5, 8)),
           b = 7) ## 36 warunków

gui.show.instruction("Dziękujemy za udział w badaniu. Proszę poczekać na swoim miejscu, aż osoba prowadząca badanie podejdzie i poinformuje o dalszym postępowaniu.")

if(!interactive())quit("no")
