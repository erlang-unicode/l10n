-record(comment, 
    {line      :: non_neg_integer(), 
     text = [] :: [string()]
    }).

-record(l10n_call, 
    {line        :: non_neg_integer(), 
     string = [] :: string(),
     call        :: atom()
    }).

-record(value, 
    {
     call         :: atom(),
     string  = [] :: string(),
     place   = [] :: iolist(),
     comment = [] :: [string()]
    }).

-record(value2, 
    {
     string  = [] :: string(), % id
     place   = [] :: [iolist()],
     comment = [] :: [string()]
    }).

-record(trans, 
    {
     id      = [] :: string(),
     string  = [] :: string(),
     comment = [] :: [string()] % Translator's comments
    }).

