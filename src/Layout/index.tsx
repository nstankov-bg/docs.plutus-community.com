import { Box } from "@chakra-ui/react";
import React from "react";
import { Navigation } from "../components";
import TopNav from "../components/TopNav";
import useScreenWiderThanBreakpoint from "../hooks/useScreenWiderThanBreakpoint";
import Markdown from "../Markdown";
import { navComponentMapping } from "../utils/componentMapping/navigation";

const Layout: React.FC<{
  navigation?: string;
}> = ({ children, navigation }) => {
  const isWideXl = useScreenWiderThanBreakpoint(1920);
  const isWide = useScreenWiderThanBreakpoint(1200);

  let width: number | string = "100%";

  if (isWideXl) {
    width = 1200;
  } else if (isWide) {
    width = 900;
  }

  return (
    <Box d="flex" h="100vh" w="100%" flexDirection="column">
      <TopNav />
      <Box
        d="flex"
        position="relative"
        w="100%"
        css={{ height: "calc(100% - 52px)" }}
      >
        {navigation && (
          <Navigation>
            <Markdown componentMapping={navComponentMapping}>
              {navigation}
            </Markdown>
          </Navigation>
        )}
        <Box
          as="main"
          flex="1"
          display="flex"
          justifyContent="center"
          css={{
            overflowY: "auto",
          }}
        >
          <Box
            p="12"
            css={{
              boxSizing: "border-box",
              lineHeight: 1.6,
              width,
            }}
          >
            {children}
          </Box>
        </Box>
      </Box>
    </Box>
  );
};

export default Layout;
