-define(LSTR(X), 
		L10N_HASH = erlang:phash2(X), 
		L10N_LOCALE = l10n_locale:get_locale(), 
		try 
			L10N_STR = l10n_store:search_string(L10N_LOCALE, L10N_HASH),
			true = is_binary(L10N_STR)
		catch error:_ -> l10n_store:load_string(L10N_LOCALE, X)
		end).

-define(LMSG(X, Params), 
		L10N_HASH = erlang:phash2(X), 
		L10N_LOCALE = l10n_locale:get_locale(), 
		L10N_MSG = try  
			L10N_STR = l10n_store:search_format(L10N_LOCALE, L10N_HASH),
			true = is_binary(L10N_STR)
		catch error:_ -> l10n_store:load_format(L10N_LOCALE, X)
		end,
		i18n_message:format(L10N_MSG, Params)).
