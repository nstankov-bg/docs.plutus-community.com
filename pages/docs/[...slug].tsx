import path from "path";
import { getBookPath } from "../../src/utils/getBookPath";
import { getDocSlugs } from "../../src/utils/getDocsWithSlugs";
import { GetStaticProps, GetStaticPaths } from "next";
import { getNavigationSource } from "../../src/utils/getNavigationData";
import { getDocSource } from "../../src/utils/getDocSource";
import Layout from "../../src/Layout";
import React from "react";
import Markdown from "../../src/Markdown";
import { mainComponentMapping } from "../../src/utils/componentMapping/main";

const Doc: React.FC<{
  source: string;
  navigation: string;
}> = ({ source, navigation }) => {
  return (
    <Layout navigation={navigation}>
      <Markdown componentMapping={mainComponentMapping}>{source}</Markdown>
    </Layout>
  );
};

export const getStaticPaths: GetStaticPaths = async () => {
  const bookPath = getBookPath();
  const docsPath = path.join(bookPath, "docs");
  const slugs = await getDocSlugs(docsPath, []);

  const paths = slugs.map((slug) => ({
    params: {
      slug: slug.split("/"),
    },
  }));

  return {
    paths,
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const { slug } = params;

  let filePath: string;
  if (typeof slug === "string") {
    filePath = slug;
  }
  // if file is nested in a directory
  filePath = (slug as string[]).join("/");
  // Fetch and parse markdown for the given file
  const bookPath = getBookPath();
  const docFilePath = path.join(bookPath, "docs", filePath);

  const source = getDocSource(docFilePath);
  const navigation = getNavigationSource(bookPath);

  return { props: { source, navigation } };
};

export default Doc;
