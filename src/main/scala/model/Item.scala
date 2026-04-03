package model

case class Item(
    title: String,
    guids: List[String],
    category: String,
    ended: Option[Boolean] = None,
    genres: Set[String] = Set.empty
) {
  def mergeWith(that: Item): Item =
    Item(
      title = if (this.title.nonEmpty) this.title else that.title,
      guids = (this.guids ++ that.guids).distinct,
      category = this.category,
      ended = this.ended.orElse(that.ended),
      genres = this.genres ++ that.genres
    )

  def getTvdbId: Option[Long] =
    guids.find(_.startsWith("tvdb://")).flatMap(_.stripPrefix("tvdb://").toLongOption)

  def getTmdbId: Option[Long] =
    guids.find(_.startsWith("tmdb://")).flatMap(_.stripPrefix("tmdb://").toLongOption)

  def getRadarrId: Option[Long] =
    guids.find(_.startsWith("radarr://")).flatMap(_.stripPrefix("radarr://").toLongOption)

  def getSonarrId: Option[Long] =
    guids.find(_.startsWith("sonarr://")).flatMap(_.stripPrefix("sonarr://").toLongOption)

  def hasAnyGenre(candidates: Set[String]): Boolean = {
    val normalizedGenres     = genres.map(_.trim.toLowerCase)
    val normalizedCandidates = candidates.map(_.trim.toLowerCase)
    normalizedGenres.intersect(normalizedCandidates).nonEmpty
  }

  def matches(that: Any): Boolean = that match {
    case Item(_, theirGuids, c, _, _) if c == this.category =>
      theirGuids.foldLeft(false) { case (acc, guid) =>
        acc || guids.contains(guid)
      }
    case _ => false
  }
}
