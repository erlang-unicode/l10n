-record(comment, 
    {line      :: non_neg_integer(), 
     text = [] :: [string()]
    }).

-record(macro, 
    {line        :: non_neg_integer(), 
     string = [] :: string(),
     macro       :: atom()
    }).

-record(value, 
    {
     macro   :: atom(),
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

