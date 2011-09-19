
-compile({parse_transform, l10n_expand}). 

-define(LTXT(X), 
	begin
		L10N_HASH = ct_expand:term( 
			erlang:phash2(X)
		), 
		try 
			l10n_store_server:search_string(L10N_HASH)
		catch error:_ -> l10n_store_server:load_string(X)
		end
	end).

-define(LFMT(X, Params), 
	begin
		L10N_HASH = ct_expand:term(
			erlang:phash2(X)
		), 
		L10N_MSG = try  
			l10n_store_server:search_format(L10N_HASH)
		catch error:_ -> l10n_store_server:load_format(X)
		end,
		i18n_message:format(L10N_MSG, Params)
	end).
