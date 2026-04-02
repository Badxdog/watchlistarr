package model

case class Item(
    title: String,
    guids: List[String],
    category: String,
    ended: Option[Boolean] = None,
    genres: Set[String] = Set.empty
) {
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
