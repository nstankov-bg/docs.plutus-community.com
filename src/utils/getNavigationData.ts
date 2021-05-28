import path from "path";
import fs from "fs";

export function getNavigationSource(bookPath: string) {
  const summaryPath = path.join(bookPath, "SUMMARY.md");
  return fs.readFileSync(summaryPath).toString();
}
