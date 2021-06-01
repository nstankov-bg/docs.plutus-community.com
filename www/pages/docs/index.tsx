import path from "path";
import { getBookPath } from "../../src/utils/getBookPath";
import Layout from "../../src/Layout";
import React from "react";
import Markdown from "../../src/Markdown";
import { mainComponentMapping } from "../../src/utils/componentMapping/main";
import { getDocSource } from "../../src/utils/getDocSource";
import { getNavigationSource } from "../../src/utils/getNavigationData";

const Docs: React.FC<{
  source: string;
  navigation: string;
}> = ({ source, navigation }) => {
  return (
    <Layout navigation={navigation}>
      <Markdown componentMapping={mainComponentMapping}>{source}</Markdown>
    </Layout>
  );
};

export async function getStaticProps() {
  const bookPath = getBookPath();

  const readmePath = path.join(bookPath, "README.md");

  const source = getDocSource(readmePath);
  const navigation = getNavigationSource(bookPath);

  return { props: { source, navigation } };
}

export default Docs;
