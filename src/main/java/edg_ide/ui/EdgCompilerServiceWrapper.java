package edg_ide.ui;

import com.intellij.openapi.project.Project;


// Needed separate from the Scala superclass because IntelliJ doesn't seem to like Scala generated code
public class EdgCompilerServiceWrapper extends EdgCompilerService {
  public EdgCompilerServiceWrapper(Project project) {
    super(project);
  }
}
