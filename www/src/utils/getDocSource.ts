import fs from "fs";

export const getDocSource = (docFilePath: string) => {
  return fs.readFileSync(docFilePath).toString();
};
