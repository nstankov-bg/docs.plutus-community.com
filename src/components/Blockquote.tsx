import { useColorModeValue } from "@chakra-ui/color-mode";

export const Blockquote: React.FC = (props) => {
  const backgroundColor = useColorModeValue("#edf2f7", "gray.600");
  const color = useColorModeValue("black", "gray.100");

  return (
    <blockquote
      css={{
        backgroundColor,
        color,
        borderLeft: "4px solid darkgrey",
        padding: "4px 4px 4px 12px",
      }}
      {...props}
    />
  );
};
