import { useColorModeValue } from "@chakra-ui/react";
import React from "react";

import SyntaxHighlighter from "react-syntax-highlighter";
import {
  atelierSulphurpoolLight,
  atomOneDark,
} from "react-syntax-highlighter/dist/cjs/styles/hljs";

export const Pre: React.FC = (props) => {
  const codeElement = props?.children[0] as JSX.Element;

  const languageString = codeElement.props?.className;
  const language = languageString
    ? (languageString.split("language-")[1] as string)
    : null;

  const codeString = codeElement.props?.children[0];

  const style = useColorModeValue(atelierSulphurpoolLight, atomOneDark);

  return (
    <SyntaxHighlighter
      language={language}
      css={{
        color: "white",
        borderRadius: 4,
        padding: "20px 16px 0px 16px !important",
        marginTop: 24,
        marginBottom: 24,
        "& > code": {
          marginTop: 36,
        },
      }}
      style={style}
    >
      {codeString}
    </SyntaxHighlighter>
  );
};
