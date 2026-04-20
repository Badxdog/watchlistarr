package plex

private[plex] case class TokenWatchlistItem(
    title: String,
    guid: String,
    `type`: String,
    key: String,
    Guid: List[Guid] = List.empty,
    Genre: List[Genre] = List.empty
)

private[plex] case class Guid(id: String)
private[plex] case class Genre(tag: String)
