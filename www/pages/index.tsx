import { Heading } from "@chakra-ui/layout";
import { A } from "../src/components";
import { useEffect, useRef } from "react";
import stars from "../src/sketches/stars";
import Layout from "../src/Layout";
import useScreenWiderThanBreakpoint from "../src/hooks/useScreenWiderThanBreakpoint";
import Link from "next/link";

const Home: React.FC = () => {
  const p5Ref = useRef();
  const isWide = useScreenWiderThanBreakpoint(768);

  useEffect(() => {
    const p5 = require("p5");
    new p5(stars, p5Ref.current);
  }, []);

  return (
    <Layout>
      <div
        ref={p5Ref}
        css={{
          position: "absolute",
          top: 0,
          left: 0,
        }}
      />
      <div
        css={{
          position: "absolute",
          top: 0,
          left: 0,
          display: "flex",
          flexShrink: 0,
          width: "100%",
          height: "100%",
          flexDirection: "column",
          justifyContent: "center",
          alignItems: "center",
          textAlign: "center",
        }}
      >
        <Heading
          fontSize={isWide ? "72" : "64"}
          m="0"
          pt="0"
          pb="8"
          fontWeight="bold"
        >
          Plutus Community
        </Heading>
        <Link href="/docs" passHref>
          <A css={{ fontSize: 24 }}>Docs</A>
        </Link>
      </div>
    </Layout>
  );
};

export default Home;
