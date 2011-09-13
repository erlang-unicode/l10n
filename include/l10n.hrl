-define(LSTR(X), 
		L10N_HASH = erlang:phash2(X), 
		L10N_LOCALE = l10n_locale:get_locale(), 
		try l10n_string_store:search(L10N_HASH) 
		catch error:_ -> X
		end).

-define(LMSG(X, Params), 
		L10N_HASH = erlang:phash2(X), 
		L10N_LOCALE = l10n_locale:get_locale(), 
		L10N_MSG = try l10n_message_store:search(L10N_HASH) 
		catch error:_ -> l10n_message_store:compile(X)
		end,
		i18n_message:format(L10N_MSG, Params)).
