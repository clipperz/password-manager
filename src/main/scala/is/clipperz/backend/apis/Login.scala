package is.clipperz.backend.apis

import java.util

import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.LogAspect
import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ fromStream, responseTimer }
import is.clipperz.backend.services.{ SessionManager, SrpManager, SRPStep1Data, SRPStep2Data }
import is.clipperz.backend.exceptions.{ BadRequestException, FailedConversionException, ResourceNotFoundException }

import zio.{ Cause, Chunk, Task, ZIO }
import zio.durationInt
import zio.metrics.Metric
import zio.json.EncoderOps
import zio.http.{ Method, Path, Response, Request, Status }
import zio.http.Header.HeaderType
import zio.http.*
import zio.http.endpoint.openapi.OpenAPIGen
import zio.http.endpoint.Endpoint
import zio.json.JsonEncoder
import is.clipperz.backend.services.SRPStep1Response
import zio.json.ast.Json

val loginApi: Routes[SessionManager & SrpManager, Throwable] = Routes(
    Method.POST / "api" / "login" / "step1" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("login.step1", request.method)(
            ZIO
            .service[SessionManager]
            .zip(ZIO.service[SrpManager])
            .zip(ZIO.succeed(request.body.asStream)).tap(_ => ZIO.logInfo(s"STEP 1 - c:$c"))
            .flatMap((sessionManager, srpManager, content) =>
                fromStream[SRPStep1Data](content)
                    .flatMap { loginStep1Data =>
                        if HexString(c) == loginStep1Data.c then
                            for {
                                session <- sessionManager.getSession(request) // create new session
                                (step1Response, session) <- srpManager.srpStep1(loginStep1Data, session)
                                _ <- sessionManager.saveSession(session)
                            } yield step1Response
                        else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
                    }
            )
            .map(step1Response => Response.json(step1Response.toJson))
        ) @@ LogAspect.logAnnotateRequestData(request)
,
    Method.POST / "api" / "login" / "step2" / string("c") -> handler: (c: String, request: Request) =>
        responseTimer("login.step2", request.method)(  
            ZIO
            .service[SessionManager]
            .zip(ZIO.service[SrpManager])
            .zip(ZIO.succeed(request.body.asStream))
            .flatMap((sessionManager, srpManager, content) =>
                fromStream[SRPStep2Data](content)
                    .flatMap { loginStep2Data =>
                    for {
                        session <- sessionManager.getSession(request)
                        (step2Response, session) <- srpManager.srpStep2(loginStep2Data, session)
                        _ <- sessionManager.saveSession(session)
                    } yield step2Response
                    }
            )
            .map(step2Response => Response.json(step2Response.toJson))
        ) @@ LogAspect.logAnnotateRequestData(request)
)