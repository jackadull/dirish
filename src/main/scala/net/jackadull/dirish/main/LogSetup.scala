package net.jackadull.dirish.main

import ch.qos.logback.classic.Level.{INFO, WARN}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.core.ConsoleAppender
import org.slf4j.Logger.ROOT_LOGGER_NAME
import org.slf4j.LoggerFactory

object LogSetup {
  def apply() {
    createNewConsoleLogger()
    setLogLevel(ROOT_LOGGER_NAME, INFO)
    setLogLevel("org.eclipse.jgit", WARN)
  }

  private def createNewConsoleLogger() {
    val rootLogger = getLogger(ROOT_LOGGER_NAME)

    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val oldConsoleAppenderOrNull = rootLogger.getAppender("console")

    Option(oldConsoleAppenderOrNull) foreach {oldConsoleAppender ⇒
      oldConsoleAppender.stop()
      rootLogger.detachAppender("console")

      val encoder = new PatternLayoutEncoder
      encoder.setPattern("[%highlight(%level)] %msg%n")
      encoder.setContext(loggerContext)
      encoder.start()

      val newAppender = new ConsoleAppender[ILoggingEvent]
      newAppender.setEncoder(encoder)
      newAppender.setContext(loggerContext)
      newAppender.start()

      rootLogger.addAppender(newAppender)
    }
  }

  private def setLogLevel(logger:String, level:Level) {getLogger(logger).setLevel(level)}

  private def getLogger(logger:String):Logger = LoggerFactory.getLogger(logger).asInstanceOf[Logger]
}
