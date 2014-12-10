-type worker() :: pid().

-type table() :: items | unique_items | lists
                 | users | devices | user_groups | user_usergroup
                 | {table(), join, table(), on, {column_spec(), comp(), column_spec()}}.
-type column_spec() :: {table(), atom()}.

-type filter() :: [filter_cond()].
-type filter_cond() :: {atom(), term()} | {atom(), in, list()} | {atom(), comp(), term()} | {'not', 'exists', {select, atom(), {from, atom()}, tuple()}}.
-type comp() :: '>' | '<' | '=' | '<>'.

-type update_filter() :: table() | {table(), filter_cond()} | {table(), filter()}.

-type returns() :: [] | atom() | '*'.

-type values() :: [{atom(), term()}].