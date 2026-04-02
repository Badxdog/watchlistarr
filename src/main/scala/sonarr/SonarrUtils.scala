package sonarr

import cats.data.EitherT
import cats.effect.IO
import configuration.{SonarrCategoryOverride, SonarrConfiguration}
import http.HttpClient
import io.circe.{Decoder, Json}
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import model.Item
import org.http4s.{MalformedMessageBodyFailure, Method, Uri}
import org.slf4j.LoggerFactory

trait SonarrUtils extends SonarrConversions {

  private val logger = LoggerFactory.getLogger(getClass)

  protected def fetchSeries(
      client: HttpClient
  )(apiKey: String, baseUrl: Uri, bypass: Boolean): EitherT[IO, Throwable, Set[Item]] =
    for {
      shows <- getToArr[List[SonarrSeries]](client)(baseUrl, apiKey, "series")
      exclusions <-
        if (bypass) {
          EitherT.pure[IO, Throwable](List.empty[SonarrSeries])
        } else {
          getToArr[List[SonarrSeries]](client)(baseUrl, apiKey, "importlistexclusion")
        }
    } yield (shows.map(toItem) ++ exclusions.map(toItem)).toSet

  protected def addToSonarr(client: HttpClient)(config: SonarrConfiguration)(item: Item): IO[Unit] = {
    val maybeCategoryOverride = selectCategoryOverride(config, item)
    val qualityProfileId      = maybeCategoryOverride.map(_.qualityProfileId).getOrElse(config.sonarrQualityProfileId)
    val rootFolder            = maybeCategoryOverride.map(_.rootFolder).getOrElse(config.sonarrRootFolder)

    val addOptions = SonarrAddOptions(config.sonarrSeasonMonitoring)
    val show = SonarrPost(
      item.title,
      item.getTvdbId.getOrElse(0L),
      qualityProfileId,
      rootFolder,
      addOptions,
      config.sonarrLanguageProfileId,
      tags = config.sonarrTagIds.toList
    )

    val result = postToArr[Unit](client)(config.sonarrBaseUrl, config.sonarrApiKey, "series")(show.asJson)
      .fold(
        err => logger.debug(s"Received warning for sending ${item.title} to Sonarr: $err"),
        result => result
      )

    result.map { r =>
      logger.info(s"Sent ${item.title} to Sonarr")
      r
    }
  }

  private def selectCategoryOverride(config: SonarrConfiguration, item: Item): Option[SonarrCategoryOverride] = {
    val maybeRule = config.sonarrCategoryOverrides.find(rule => item.hasAnyGenre(rule.genres))
    maybeRule.foreach { rule =>
      logger.info(s"Matched Sonarr category override '${rule.name}' for ${item.title}")
    }
    maybeRule
  }

  protected def deleteFromSonarr(client: HttpClient, config: SonarrConfiguration, deleteFiles: Boolean)(
      item: Item
  ): EitherT[IO, Throwable, Unit] = {
    val showId = item.getSonarrId.getOrElse {
      logger.warn(s"Unable to extract Sonarr ID from show to delete: $item")
      0L
    }

    deleteToArr(client)(config.sonarrBaseUrl, config.sonarrApiKey, showId, deleteFiles)
      .map { r =>
        logger.info(s"Deleted ${item.title} from Sonarr")
        r
      }
  }

  private def deleteToArr(
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, id: Long, deleteFiles: Boolean): EitherT[IO, Throwable, Unit] = {
    val urlWithQueryParams = (baseUrl / "api" / "v3" / "series" / id)
      .withQueryParam("deleteFiles", deleteFiles)
      .withQueryParam("addImportListExclusion", false)

    EitherT(client.httpRequest(Method.DELETE, urlWithQueryParams, Some(apiKey)))
      .recover { case _: MalformedMessageBodyFailure => Json.Null }
      .map(_ => ())
  }

  private def getToArr[T: Decoder](
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, endpoint: String): EitherT[IO, Throwable, T] =
    for {
      response     <- EitherT(client.httpRequest(Method.GET, baseUrl / "api" / "v3" / endpoint, Some(apiKey)))
      maybeDecoded <- EitherT.pure[IO, Throwable](response.as[T])
      decoded <- EitherT.fromOption[IO](maybeDecoded.toOption, new Throwable("Unable to decode response from Sonarr"))
    } yield decoded

  private def postToArr[T: Decoder](
      client: HttpClient
  )(baseUrl: Uri, apiKey: String, endpoint: String)(payload: Json): EitherT[IO, Throwable, T] =
    for {
      response <- EitherT(
        client.httpRequest(Method.POST, baseUrl / "api" / "v3" / endpoint, Some(apiKey), Some(payload))
      )
      maybeDecoded <- EitherT.pure[IO, Throwable](response.as[T])
      decoded <- EitherT.fromOption[IO](maybeDecoded.toOption, new Throwable("Unable to decode response from Sonarr"))
    } yield decoded
}
