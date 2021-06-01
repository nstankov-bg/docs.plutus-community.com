import path from 'path';

export function getBookPath() {
  return path.join(process.cwd(), "book");
}