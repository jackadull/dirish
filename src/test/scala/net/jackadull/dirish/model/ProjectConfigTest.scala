package net.jackadull.dirish.model

import java.util.UUID

import net.jackadull.dirish.path.{PathElementSpec, RelativePathSpec, UserHomePathSpec}
import org.scalatest.{FreeSpec, Matchers}

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}

class ProjectConfigTest extends FreeSpec with Matchers {
  private val home = UserHomePathSpec
  private implicit def toRelativePathSpec(str:String):RelativePathSpec = PathElementSpec(str)
  private implicit def toUUID(str:String):UUID = UUID.fromString(str)

  "applying changes to an empty project config, then undoing those changes again, results in the empty project config" in {
    var cfg:ProjectConfig = ProjectConfig.empty
    cfg = applyChangesExpectingSuccess(cfg, simpleChangeSeq1)
    cfg should not be (ProjectConfig empty)
    cfg = applyChangesExpectingSuccess(cfg, simpleChangeSeq1Reverse)
    cfg should be (ProjectConfig empty)
  }

  "applying the changes between a config and the empty config to the config results in an empty config" in {
    val cfg:ProjectConfig = applyChangesExpectingSuccess(ProjectConfig.empty, simpleChangeSeq1)
    val cfg2:ProjectConfig = applyChangesExpectingSuccess(cfg, allChanges(ProjectConfig.changesBetween(cfg, ProjectConfig.empty)))
    cfg2 should be (ProjectConfig empty)
  }

  "applying the changes between the empty config and another config to the empty config results in the non-empty config" in {
    val cfg:ProjectConfig = applyChangesExpectingSuccess(ProjectConfig.empty, simpleChangeSeq1)
    val cfg2:ProjectConfig = applyChangesExpectingSuccess(ProjectConfig.empty, allChanges(ProjectConfig.changesBetween(ProjectConfig.empty, cfg)))
    cfg2 should be (cfg)
  }

  private def applyChangesExpectingSuccess(conf:ProjectConfig, changes:Seq[ConfigChangeSpec]):ProjectConfig = applyChanges(conf, changes) match {
    case Left((change, error)) ⇒ fail(s"error applying change '$change': $error")
    case Right(newCfg) ⇒ newCfg
  }

  @tailrec private def applyChanges(conf:ProjectConfig, changes:Seq[ConfigChangeSpec]):Either[(ConfigChangeSpec,ConfigChangeError),ProjectConfig] = changes match {
    case Seq(fst,rst@_*) ⇒ fst applyTo conf match {
      case Left(err) ⇒ Left(fst → err)
      case Right(conf2) ⇒ applyChanges(conf2, rst)
    }
    case Seq() ⇒ Right(conf)
  }

  private def allChanges(stage:ConfigChangeStage):Seq[ConfigChangeSpec] = stage match {
    case s:ConfigChangeStages ⇒ allChanges(s nextStage)
    case s:GitRemotesRemovedStage ⇒ s.gitRemotesRemoved.toSeq ++ allChanges(s nextStage)
    case s:GitRepositoriesRemovedStage ⇒ s.gitRepositoriesRemoved.toSeq ++ allChanges(s nextStage)
    case s:ProjectActiveSignalsRemovedStage ⇒ s.projectActiveSignalsRemoved.toSeq ++ allChanges(s nextStage)
    case s:ProjectsRemovedStage ⇒ s.projectsRemoved.toSeq ++ allChanges(s nextStage)
    case s:BaseDirectoriesRemovedStage ⇒ s.baseDirectoriesRemoved.toSeq ++ allChanges(s nextStage)
    case s:GitFirstRemotesChangedStage ⇒ s.gitFirstRemotesChanged.toSeq ++ allChanges(s nextStage)
    case s:ProjectsMovedStage ⇒ s.projectsMoved.toSeq ++ allChanges(s nextStage)
    case s:BaseDirectoriesMovedStage ⇒ s.baseDirectoriesMoved.toSeq ++ allChanges(s nextStage)
    case s:BaseDirectoriesAddedStage ⇒ s.baseDirectoriesAdded.toSeq ++ allChanges(s nextStage)
    case s:ProjectsAddedStage ⇒ s.projectsAdded.toSeq ++ allChanges(s nextStage)
    case s:ProjectActiveSignalsAddedStage ⇒ s.projectActiveSignalsAdded.toSeq ++ allChanges(s nextStage)
    case s:GitRepositoriesAddedStage ⇒ s.gitRepositoriesAdded.toSeq ++ allChanges(s nextStage)
    case s:GitRemotesAddedStage ⇒ s.gitRemotesAdded.toSeq
  }

  val simpleChangeSeq1:Seq[ConfigChangeSpec] = Seq(
    BaseDirectoryAddedSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", home/"tst"/"prj"),
    BaseDirectoryAddedSpec("5f4fcb58-be6a-45d1-b15f-625e8ac70283", home/"tst2"),
    ProjectAddedSpec("2582c3b4-2e99-4c4f-a351-6d40246bbc2d", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "foo"/"prj1")),
    ProjectAddedSpec("2c58c40d-71e9-4628-9e7b-6f6e1d8077e6", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "foo"/"prj2")),
    ProjectAddedSpec("a2cbc854-dce6-4240-bc43-b52a32d5c5cf", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "prj3")),
    ProjectAddedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", ProjectLocationSpec("5f4fcb58-be6a-45d1-b15f-625e8ac70283", "prj3")),
    GitRepositoryAddedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", "origin" → "foobar"),
    GitRepositoryAddedSpec("2c58c40d-71e9-4628-9e7b-6f6e1d8077e6", "origin" → "foobar"),
    GitRemoteAddedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", "alt" → "foobar"),
    GitRemoteAddedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", "downstream" → "foobar2")
  )
  val simpleChangeSeq1Reverse:Seq[ConfigChangeSpec] = Seq(
    GitRemoteRemovedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", "downstream"),
    GitRemoteRemovedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", "alt"),
    GitRepositoryRemovedSpec("2c58c40d-71e9-4628-9e7b-6f6e1d8077e6"),
    GitRepositoryRemovedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8"),
    ProjectRemovedSpec("39e0c1d2-1f37-4e32-a544-537de4585ba8", ProjectLocationSpec("5f4fcb58-be6a-45d1-b15f-625e8ac70283", "prj3")),
    ProjectRemovedSpec("a2cbc854-dce6-4240-bc43-b52a32d5c5cf", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "prj3")),
    ProjectRemovedSpec("2c58c40d-71e9-4628-9e7b-6f6e1d8077e6", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "foo"/"prj2")),
    ProjectRemovedSpec("2582c3b4-2e99-4c4f-a351-6d40246bbc2d", ProjectLocationSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", "foo"/"prj1")),
    BaseDirectoryRemovedSpec("5f4fcb58-be6a-45d1-b15f-625e8ac70283", home/"tst2"),
    BaseDirectoryRemovedSpec("3a974981-ed3a-4491-bec9-10409b83d5b2", home/"tst"/"prj")
  )
}
