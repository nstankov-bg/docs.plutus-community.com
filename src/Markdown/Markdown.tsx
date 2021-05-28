import unified from "unified";
import markdown from "remark-parse";
import remark2rehype from "remark-rehype";
import rehype2react from "rehype-react";
import React from "react";

const Markdown: React.FC<{
  children: string;
  componentMapping?: Record<string, any>;
}> = ({ children, componentMapping = {} }) => {
  const processor = unified()
    .use(markdown)
    .use(remark2rehype)
    .use(rehype2react, {
      createElement: React.createElement,
      components: componentMapping,
    });

  return (
    <div css={{ paddingBottom: 24 }}>
      {processor.processSync(children).result}
    </div>
  );
};

export default Markdown;
