package plex

import model.Item

private[plex] case class Watchlist(items: Set[WatchlistItem]) {
  def toItems: Set[Item] = items.map(_.toItem)
}

private[plex] case class WatchlistItem(
    title: String,
    guids: List[String],
    category: String,
    keywords: List[String] = List.empty
) {
  def toItem: Item =
    Item(
      title = title,
      guids = guids,
      category = category,
      genres = keywords.map(_.trim).filter(_.nonEmpty).toSet
    )
}
