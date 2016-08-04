data JoinList m a = Empty
| Single m a
| Append m (JoinList m a) (JoinList m a)
deriving (Eq, Show)
