import { MoonIcon, SunIcon } from "@chakra-ui/icons";
import { Box, Button, useColorMode } from "@chakra-ui/react";
import React from "react";
import { Navigation } from "../components";
import useScreenWiderThanBreakpoint from "../hooks/useScreenWiderThanBreakpoint";
import Markdown from "../Markdown";
import { navComponentMapping } from "../utils/componentMapping/navigation";

const Layout: React.FC<{
  navigation?: string;
}> = ({ children, navigation }) => {
  const { toggleColorMode, colorMode } = useColorMode();
  const isWideXl = useScreenWiderThanBreakpoint(1920);
  const isWide = useScreenWiderThanBreakpoint(1200);

  let width: number | string = "100%";

  if (isWideXl) {
    width = 1200;
  } else if (isWide) {
    width = 900;
  }

  return (
    <Box d="flex" h="100vh" w="100%" position="relative">
      <Button
        title="toggle-color-mode"
        css={{
          position: "absolute",
          top: 8,
          right: isWide ? 32 : 16,
          zIndex: 99,
          padding: 8,
          borderRadius: "50%",
          boxShadow: "1px 0px 6px 1px rgba(0,0,0,0.2)",
        }}
        onClick={toggleColorMode}
      >
        {colorMode === "dark" ? <MoonIcon /> : <SunIcon color="yellow.500" />}
      </Button>
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
  );
};

export default Layout;
