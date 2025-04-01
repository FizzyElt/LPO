type note =
  | Do
  | Re
  | Mi
  | Fa
  | Sol
  | La
  | Si

type pitch =
  { note : note;
    octave : int
  }

type duration =
  | Half
  | Quarter

type symbol =
  | Note of pitch * duration
  | Rest of duration

type score =
  { symbols : symbol list;
    metronome : int
  }

let frequency { note = n; octave = o } =
  let f0 =
    match n with
    | Do -> 33
    | Re -> 37
    | Mi -> 41
    | Fa -> 44
    | Sol -> 49
    | La -> 55
    | Si -> 62
  in
  f0 * truncate (2. ** float o)
;;

let milliseconds d t =
  let quarter = 60000 / t in
  match d with
  | Half -> quarter * 2
  | Quarter -> quarter
;;

let sound t s =
  match s with
  | Note (p, d) ->
    let f = frequency p in
    Graphics.sound f (milliseconds d t)
  | Rest r -> Graphics.sound 0 (milliseconds r t)
;;

let play_score { symbols = l; metronome = t } = List.iter (sound t) l
