import path from "path";
import fs from "fs";

function isDir(path: string) {
  return fs.lstatSync(path).isDirectory();
}

function isMarkdownFile(path: string) {
  return path.endsWith(".md");
}

function extractSlugFromFilePath(filePath: string) {
  return filePath.split("book/docs/")[1];
}

export async function getDocSlugs(docsPath: string, slugs: string[] = []) {
  if (!isDir(docsPath) && !isMarkdownFile(docsPath)) {
    throw new Error(
      `unsupported file format -> ${docsPath}. Make sure that your doc file extensions is .md`
    );
  }
  // if the path is an .md file, we push it into the array
  if (!isDir(docsPath) && isMarkdownFile(docsPath)) {
    // const content = await parseMarkdown(docsPath);
    slugs.push(extractSlugFromFilePath(docsPath));
    return slugs;
  }
  // if docsPath is a directory we recursively check further
  const docsDir = fs.readdirSync(docsPath);

  for (const file of docsDir) {
    const filePath = path.join(docsPath, file);
    await getDocSlugs(filePath, slugs);
  }

  return slugs;
}
